using System;
using System.Windows.Forms;
using System.IO;
using System.Collections.Generic;

namespace SLAU
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
            SolveButton.Enabled = true;
        }
        public int n=2; //Размерности
        public double[,] A, A1; //Исходная, единичная
        public double[] b = new double[2];  //Вектор b
        public double[] solution; //Для решения
        public int[] kol;
        const double E = 0.0001; //Точность
        public double chis;
        List<string> strList = new List<string>(); 

        //Вывод вектора
        public void Write(double[] vec)
        {
            for (int i = 0; i < n; i++)
            {
                string str1 = "";
                str1 = String.Format("{0,-18}", vec[i]);
                strList.Add(str1);
            }
            strList.Add("");
        }

        //Выводим матрицу
        public void WriteMas(double[,] Mas)
        {
            if (Mas == A)
                strList.Add("Матрица А:");
            for (int i = 0; i < n; i++)
            {
                string str1 = "";
                for (int j = 0; j < n; j++)
                    str1 += String.Format("{0,-30} ", Mas[i, j]);
                strList.Add(str1);
            }
            strList.Add("");
        }

        //Открываем файл с матрицей
        private void Open_Click(object sender, EventArgs e)
        {
            try
            {
                OpenFileDialog a = new OpenFileDialog();
                a.Filter = "Текстовый(*.txt)|*.txt";
                a.DefaultExt = "txt";
                if (a.ShowDialog() == DialogResult.OK)
                {
                    strList.Clear();
                    SolveBox.Items.Clear();
                    SolveButton.Enabled = false;

                    string[] str = File.ReadAllLines(a.FileName);
                    n = str.Length;
                    A = new double[n, n];
                    kol = new int[n];
                    for (int i = 0; i < str.Length; i++)
                    {
                        string[] str2 = str[i].Split(' ');
                        for (int j = 0; j < n; j++)
                            A[i, j] = Convert.ToDouble(str2[j]);
                    }

                    A1 = Reverse(A);
                    SolveBox.Items.Clear();
                    SolveBox.Items.Add("Данные успешно считаны");
                    WriteMas(A);
                    strList.Add("Обратная матрица:");
                    WriteMas(A1);               
                    chis = Norma(A) * Norma(A1);
                    strList.Add(String.Format("Число обусловленности: {0}",chis));
                    SolveButton.Enabled = true;
                }
            }
            catch
            {
                MessageBox.Show("Входные данные имеют неверных формат! Убедитесь, что данные представлены ввиде матрицы");
            }
        }

        //Сохраняем решение в файл
        public void SaveFile()
        {
            SaveFileDialog saveFile = new SaveFileDialog();
            saveFile.Filter = "Текстовый(*.txt)|*.txt";
            saveFile.DefaultExt = "txt";
            saveFile.Title = "Сохранение решения";

            if (saveFile.ShowDialog() == DialogResult.OK)
            {
                FileStream fs = new FileStream(saveFile.FileName, FileMode.Create);
                StreamWriter st = new StreamWriter(fs);
                foreach (string str in strList)
                {
                        st.WriteLine(str);
                }
                st.Close();
            }
            SolveBox.Items.Add("Решение сохранено в " + saveFile.FileName);
        }       

        //Нахождение транспонированной матрицы
        public double[,] Tran(double[,] Mas)
        {
            double[,] C = Mas;
            for (int i = 0; i < n; i++)
                for (int j = 0; j < i; j++)
                    if (i != j)
                    {
                        double c = C[i, j];
                        C[i, j] = C[j, i];
                        C[j, i] = c;
                    }
            return C;
        }

        //Нахождение обратной матрицы
        public double[,] Reverse(double[,] Mas)
        {
            double o = Determ(Mas);
            double[,] C = new double[n, n];

            for (int i = 0; i < n; i++)
                for (int j = 0; j < n; j++)
                    C[i, j] = Mas[i, j];

            C = Tran(AlgAdd(C));
            for (int i = 0; i < n; i++)
                for (int j = 0; j < n; j++)
                    C[i, j] = C[i, j] / o;
            return C;
        }

        //Нахождение матрицы алгебраических дополнений
        public double[,] AlgAdd(double[,] Mas)
        {
            double[,] Alg = new double[n, n];
            for (int i = 0; i < n; i++)
                for (int j = 0; j < n; j++)
                    Alg[i, j] = (i + j) % 2 == 0 ? Determ(GetMinor(Mas, i, j)) : (-1) * Determ(GetMinor(Mas, i, j));
            return Alg;
        }

        //Определитель квадратной матрицы
        public double Determ(double[,] matrix)
        {
            if (matrix.GetLength(0) != matrix.GetLength(1)) throw new Exception(" Число строк в матрице не совпадает с числом столбцов");
            double det = 0;
            int Rank = matrix.GetLength(0);
            if (Rank == 1) det = matrix[0, 0];
            if (Rank == 2) det = matrix[0, 0] * matrix[1, 1] - matrix[0, 1] * matrix[1, 0];
            if (Rank > 2)
            {
                for (int j = 0; j < matrix.GetLength(1); j++)
                {
                    det += Math.Pow(-1, 0 + j) * matrix[0, j] * Determ(GetMinor(matrix, 0, j));
                }
            }
            return det;
        }

        //Найти минор
        public double[,] GetMinor(double[,] matrix, int row, int column)
        {
            if (matrix.GetLength(0) != matrix.GetLength(1)) throw new Exception(" Число строк в матрице не совпадает с числом столбцов");
            double[,] buf = new double[matrix.GetLength(0) - 1, matrix.GetLength(0) - 1];
            for (int i = 0; i < matrix.GetLength(0); i++)
                for (int j = 0; j < matrix.GetLength(1); j++)
                {
                    if ((i != row) || (j != column))
                    {
                        if (i > row && j < column) buf[i - 1, j] = matrix[i, j];
                        if (i < row && j > column) buf[i, j - 1] = matrix[i, j];
                        if (i > row && j > column) buf[i - 1, j - 1] = matrix[i, j];
                        if (i < row && j < column) buf[i, j] = matrix[i, j];
                    }
                }
            return buf;
        }

        //Разность векторов
        public double[] Subtraction(double[] vec1, double[] vec2)
        {
            double[] C = new double[n];
            for (int i = 0; i < n; i++)
                C[i] = vec1[i] - vec2[i];
            return C;
        }

        //Скалярное произведение векторов
        public double MultiScal(double[] A, double[] B)
        {
            double sum = A[0] * B[0];
            for (int i = 1; i < n; i++)
                sum += A[i] * B[i];
            return sum;
        }
        public void Delta(double i)
        {
            strList.Add("delta = " + Convert.ToString(i));
        }

        //Для умножения
        public double Sum(int i, int j, double[,] A, double[,] B)
        {
            double sum = 0;
            for (int k = 0; k < n; k++)
                sum += A[i, k] * B[k, j];
            return sum;
        }

        public double Sum(int i, double[,] A, double[] B)
        {
            double sum = 0;
            for (int k = 0; k < n; k++)
                sum += A[i, k] * B[k];
            return sum;
        }

        public double Sum(int i, double[] A, double[,] B)
        {
            double sum = 0;
            for (int k = 0; k < n; k++)
                sum += A[k] * B[k, i];
            return sum;
        }
        //умножение вектора на число
        public double[] Multiplication(double A, double[] B)
        {
            if (B != null)
            {
                double[] C = new double[n];
                for (int i = 0; i < n; i++)                    
                        C[i] = B[i]*A;
                return C;
            }
            return null;
        }

        //Умножение матрицы на вектор
        public double[] Multiplication(double[,] A, double[] B)
        {
            if (A != null && B != null)
            {
                double[] C = new double[n];
                for (int i = 0; i < n; i++)
                    for (int j = 0; j < n; j++)
                        C[i] = Sum(i, A, B);
                return C;
            }
            return null;
        }

        //Умножение матриц
        public double[,] Multiplication(double[,] A, double[,] B)
        {
            if (A != null && B != null)
            {
                double[,] C = new double[n, n];
                for (int i = 0; i < n; i++)
                    for (int j = 0; j < n; j++)
                        C[i, j] = Sum(i, j, A, B);
                return C;
            }
            return null;
        }
        //Нахождение нормы матрицы (max)
        public double Norma(double[,] mas)
        {
            double max = 0, temp = 0;
            for (int i = 0; i < n; i++)
            {
                temp = 0;
                for (int j = 0; j < n; j++)
                    temp += Math.Abs(mas[i, j]);
                if (temp > max)
                    max = temp;
            }
            return max;
        }

        //Нахождение нормы вектора (max)
        public double Norma(double[] vec)
        {
            double max = 0;
            for (int i = 0; i < n; i++)
                if (max < Math.Abs(vec[i]))
                    max = Math.Abs(vec[i]);
            return max;
        }

        public double[] f_13(double[] x)
        {
            var temp = new double[n];
            temp[0] = Math.Sin(x[1]) + 2 * x[0] - 2;
            temp[1] = Math.Cos(x[0]-1)+x[1]-0.7;            
            return temp;
        }
        public double[,] f_derivative_13(double[] x)
        {
            var temp = new double[n, n];
            temp[0, 0] = 2;
            temp[0, 1] = Math.Cos(x[1]);
            temp[1, 0] = -Math.Sin(x[0]-1);
            temp[1, 1] = 1;
            return temp;
        }
        public double[] f_derivative_13_grad(double[] x_)
        {
            double x=x_[0], y=x_[1];
            var temp = new double[n];
            temp[0] = 4*(2*x+Math.Sin(y)-2)+2*Math.Sin(1-x)*(Math.Cos(1-x)+y-0.7);
            temp[1] = 2*(Math.Cos(1-x)+y-0.7)+2*Math.Cos(y)*(2*x+Math.Sin(y)-2);
            return temp;
        }
        public double[] f_22(double[] x)
        {
            var temp = new double[n];
            temp[0] = Math.Cos(x[0]-1) + x[1] - 0.8;
            temp[1] = -Math.Cos(x[1]) + x[0] - 2;
            return temp;
        }
        public double[,] f_derivative_22(double[] x)
        {
            var temp = new double[n, n];
            temp[0, 0] = -Math.Sin(x[0] - 1);
            temp[0, 1] = 1;
            temp[1, 0] = Math.Sin(x[1]);
            temp[1, 1] = 1;
            return temp;
        }
        public double f_13_grad(double[] x_)
        {

            double temp,x,y;
            x = x_[0];
            y = x_[1];
            temp = 4 * x * x + 4 * x * (Math.Sin(y) - 2) + Math.Pow(Math.Sin(y), 2) - 4 * Math.Sin(y) + 4.99 + 2 * y * Math.Cos(1 - x) + Math.Pow(Math.Cos(1 - x), 2) - 1.4 * Math.Cos(1 - x) + y + y - 1.4 * y;      
            return temp;
        }
        public double f_22_grad(double[] x_)
        {

            double temp, x, y;
            x = x_[0];
            y = x_[1];
            temp = x * x - 2 * x * Math.Cos(y) + 2 * y * Math.Cos(1 - x) - 4 * x + Math.Pow(Math.Cos(1 - x), 2) - 1.6 * Math.Cos(1 - x) + y * y - 1.6 * y + Math.Pow(Math.Cos(y), 2) + 4 * Math.Cos(y) + 4.64;
            return temp;
        }
        public double[] f_derivative_22_grad(double[] x_)
        {
            double x, y;
            x = x_[0];
            y = x_[1];
            var temp = new double[n];
            temp[0] = 2 * (x - Math.Cos(y) - 2) + 2 * Math.Sin(1 - x) * (Math.Cos(1 - x) + y - 0.8);
            temp[1] = 2 * (Math.Cos(1 - x) + y - 0.8) + 2 * Math.Sin(y) * (x - Math.Cos(y) - 2);
            return temp;
        }
        //Метод Ньютона
        public void Niuton()
        {          
            double[] x_new, x_old,pogreshnost;
            x_new = new double[n];
            x_old = new double[n];
            
            pogreshnost = new double[n];
            double delta = 0;
            int k = 0;
            strList.Add("Метод Ньютона:");           
            if (rb_13.Checked)
            {
                x_old[0] = 1;
                x_old[1] = 0;
                strList.Add("Начальные значения: Х = " + x_old[0] + " Y = " + x_old[1]);
                strList.Add(String.Format("|  №  |        x1         |         y         |        норма      |         q         |", "№", "x1", "y"));
                strList.Add(String.Format("|{0,4} |{1,19}|{2,19}|{3,19}|", k, x_old[0], x_old[1], delta));
                do
                {
                    k++;
                    for (int i = 0; i < n; i++)
                        x_new[i] = x_old[i] - Multiplication(Reverse(f_derivative_13(x_old)), f_13(x_old))[i];
                    for (int i = 0; i < n; i++)
                        pogreshnost[i] = Math.Abs(x_new[i] - x_old[i]);
                    delta = Norma(pogreshnost);
                    x_old = (double[])x_new.Clone();
                    strList.Add(String.Format("|{0,4} |{1,19}|{2,19}|{3,19}|", k, x_old[0], x_old[1], delta));
                    
                }
                while (delta > E);
            }
            else
            {
                x_old[0] = 2;
                x_old[1] = 0;
                strList.Add("Начальные значения: Х = " + x_old[0] + " Y = " + x_old[1]);
                strList.Add(String.Format("|  №  |        x1         |         y         |        норма      |         q         |", "№", "x1", "y"));
                strList.Add(String.Format("|{0,4} |{1,19}|{2,19}|{3,19}|", k, x_old[0], x_old[1], delta));
                do
                {
                    for (int i = 0; i < n; i++)
                        x_new[i] = x_old[i] - Multiplication(Reverse(f_derivative_22(x_old)), f_22(x_old))[i];
                    for (int i = 0; i < n; i++)
                        pogreshnost[i] = Math.Abs(x_new[i] - x_old[i]);
                    delta = Norma(pogreshnost);
                    x_old = (double[])x_new.Clone();
                    strList.Add(String.Format("|{0,4} |{1,19}|{2,19}|{3,19}|", k, x_old[0], x_old[1], delta));
                    k++;
                }
                while (delta > E);
                b = (double[])x_old.Clone();
            }              

        }
        public void Grad()
        {
            double[] x_new, x_old, pogreshnost;
            x_new = new double[n];
            x_old = new double[n];
            x_old[0] = 2;
            x_old[1] = 0;
            pogreshnost = new double[n];
            double delta = 0;
            double stop,lambda,Ak_norm;
            double Ak,Ak_;
            int min = 99999, k = 0;
            strList.Add("Метод градиентного спуска:");
            if (rb_13.Checked)
            {
                x_old[0] = 1;
                x_old[1] = 0;
                Ak = 0.17;
                lambda = 0.9;
                strList.Add("Начальные значения: Х = " + x_old[0] + " Y = " + x_old[1]);
                strList.Add(String.Format("|  №  |        x         |         y         |        норма      |       альфа       |", "№", "x", "y"));
                strList.Add(String.Format("|{0,4} |{1,19}|{2,19}|{3,19}|{4,19}", k, x_old[0], x_old[1], delta, Ak));
                do
                {
                    k++;
                    var f_proizv = new double[n];
                    Ak = 0.17;
                    while (f_13_grad(Subtraction(x_old, Multiplication(Ak, f_derivative_13_grad(x_old)))) >= f_13_grad(x_old))
                        Ak = lambda * Ak;
                    f_proizv = f_derivative_13_grad(x_old);
                    f_proizv = Multiplication(Ak, f_proizv);
                    for (int i = 0; i < n; i++)
                        x_new[i] = x_old[i] - f_proizv[i];
                    for (int i = 0; i < n; i++)
                        pogreshnost[i] = Math.Abs(x_new[i] - x_old[i]);
                    delta = Norma(pogreshnost);
                    stop = Math.Abs(f_13_grad(x_new) - f_13_grad(x_old));
                    x_old = (double[])x_new.Clone();
                    strList.Add(String.Format("|{0,4} |{1,19}|{2,19}|{3,19}|{4,19}|", k, x_old[0], x_old[1], delta, Ak));

                }
                while (stop > E);
            }
            else
            {

                double lol = 10000;
                double kek, rip=0;
                Ak_ = 1;
                do
                {
                    stop = 10;
                    Ak_ = Ak_ - 0.001;
                    lambda = 0.90;
                    x_old[0] = 2;
                    x_old[1] = 0;
                    x_new[0] = 0; x_new[1] = 0;
                    k = 0;
                    do
                    {
                        k++;
                        Ak = Ak_;
                        while (f_22_grad(Subtraction(x_old, Multiplication(Ak, f_derivative_22_grad(x_old)))) >= f_22_grad(x_old))
                            Ak = lambda * Ak;
                        var f_proizv = new double[n];
                        f_proizv = f_derivative_22_grad(x_old);
                        f_proizv = Multiplication(Ak, f_proizv);
                        for (int i = 0; i < n; i++)
                            x_new[i] = x_old[i] - f_proizv[i];
                        for (int i = 0; i < n; i++)
                            pogreshnost[i] = Math.Abs(x_new[i] - x_old[i]);
                        delta = Norma(pogreshnost);
                        stop = Math.Abs(f_22_grad(x_new) - f_22_grad(x_old));
                        x_old = (double[])x_new.Clone();
                    }
                    while (stop > E);
                    kek = Norma(Subtraction(b, x_old));
                    if (kek < lol) { lol = kek; rip = Ak_; }
                }
                while (Norma(Subtraction(b, x_old)) > E && Ak_ > 0.001);
                
                lambda = 0.99;
                x_old[0] = 2;
                x_old[1] = 0;
                strList.Add("Начальные значения: Х = " + x_old[0] + " Y = " + x_old[1]);
                strList.Add(String.Format("|  №  |        x1         |         y         |        норма      |       альфа       |", "№", "x1", "y"));
                Ak = rip;
                strList.Add(String.Format("|{0,4} |{1,19}|{2,19}|{3,19}|{4,19}", k, x_old[0], x_old[1], delta, Ak));
                do
                {
                    k++;
                    Ak = rip;
                    while (f_22_grad(Subtraction(x_old, Multiplication(Ak, f_derivative_22_grad(x_old)))) >= f_22_grad(x_old))
                        Ak = lambda * Ak;
                    var f_proizv = new double[n];
                    f_proizv = f_derivative_22_grad(x_old);
                    f_proizv = Multiplication(Ak, f_proizv);
                    for (int i = 0; i < n; i++)
                        x_new[i] = x_old[i] - f_proizv[i];
                    for (int i = 0; i < n; i++)
                        pogreshnost[i] = Math.Abs(x_new[i] - x_old[i]);
                    delta = Norma(pogreshnost);
                    stop = Math.Abs(f_22_grad(x_new) - f_22_grad(x_old));
                    x_old = (double[])x_new.Clone();
                    strList.Add(String.Format("|{0,4} |{1,19}|{2,19}|{3,19}|{4,19}|", k, x_old[0], x_old[1], stop, Ak));                    
                }
                while (stop > E);
            }

        }

        //Для нормы 

        //Решение уравнения
        private void SolveButton_Click(object sender, EventArgs e)
        {
            Niuton();
            Grad();
            SaveFile();
        }

       
    }
}
