using System;
using System.Windows.Forms;

namespace SLAU
{
    public class Matrix
    {
        public int n; //Размерности
        public double[,] A; //Исходная, нижняя треугольная, верх. треугольная, обратная, перестановок
        public double[] b = { 1, 2, 3, 4 };  //Вектор b
        public double[] solution; //Для решения

        public delegate void Writer(double[,] Mas);
        public delegate void WriterB();
        public event WriterB WriteB;
        public event Writer WriteMas;
        public delegate void Counter(int i, int j);
        public event Counter Count;

        public delegate void Norm1(int i, double temp, double max);
        public event Norm1 Norma1;
        public delegate void Norm2(int i);
        public event Norm2 Norma2;
        public delegate void Norm3(double[,] Mas);
        public event Norm3 Norma3;
        public delegate void Norm4(double max);
        public event Norm4 Norma4;

        public delegate void Condition(double a, double b, double c);
        public event Condition Con;

        //Конструкторы
        public Matrix()
        {
            this.n = 0;
        }

        public Matrix(double[,] Mas)
        {
            this.n = Mas.GetLength(0);
            this.A = Mas;
        }

        //Нахождение нормы матрицы (max)
        public double Norma(double [,] mas)
        {
            Norma3(mas);
            double max = 0,temp=0;
            for (int i = 0; i < n; i++)
            {
                Norma2(i);
                temp = 0;
                for (int j = 0; j < n; j++)
                    temp += Math.Abs(mas[i, j]);
                Norma1(i, temp, max);
                if (temp > max)
                    max = temp;
            }
            Norma4(max);
            return max;
        }

        //Нахождение нормы вектора (max)
        public double Norma(double[] vec)
        {
            double max = 0;
            for (int i = 0; i < n; i++)
                if (max < Math.Abs(vec[i]))
                    max = Math.Abs(vec[i]);
            Norma4(max);
            return max;
        }

        //Нахождение числа обусловленности
        public void Condition_number()
        {
            double a =Norma(A), b = Norma(A1);
            double c = a * b;
            Con(a, b, c);
        }

        //!!Находим верктор b (Умножение неравных матриц)
        public double[] Find(double[] vector, double[,] Mas)
        {
            if (Mas != null && n != 0)
            {
                double[] g = new double[vector.Length];
                for (int i = 0; i < vector.Length; i++)
                    g[i] = vector[i];
                for (int i = 0; i < n; i++)
                {
                    double sum = 0;
                    for (int j = 0; j < n; j++)
                        sum += Mas[i, j] * vector[j];
                    g[i] = sum;
                }
                return g;
            }
            return null;
        }

        //Находим максимальный элемент в столбце
        public int Get_row_maxElement(int i, double[,] U)
        {
            int maxind = i;
            double max = Math.Abs(U[i, i]);
            for (int j = i; j < n; j++) //От i, а не от i+1, потому что иначе он выйдет за диапозон
                if (max < Math.Abs( U[j, i]))
                {
                    maxind = j;
                    max = Math.Abs(U[j, i]);
                }
            return maxind;
        }

        //Меняем строки местами
        public void Swap(int i,int row, double[,] U)
        {
            if (row != i)
            {
                if (U == A)
                    kol *= -1;
                for (int j = 0; j < n; j++)
                {
                    double c = U[i, j];
                    U[i, j] = U[row, j];
                    U[row, j] = c;
                }
            }
        }

        //Разность матриц
        public double[,] Subtraction(double[,] Mas1, double[,] Mas2)
        {
            double[,] C = new double[n, n];
            for (int i = 0; i < n; i++)
                for (int j = 0; j < n; j++)
                    C[i,j] = Mas1[i, j] - Mas2[i, j];
            return C;
        }

        //Разность векторов
        public double[] Subtraction(double[] vec1,double[] vec2)
        {
            double[] C = new double[n];
            for (int i = 0; i < n; i++)
                C[i] = vec1[i] - vec2[i];
            return C;
        }

        //Меняем элементы вектора местами
        public void Swap(int i, int row, double[] U)
        {
            if (row != i)
            {
                double c = U[i];
                U[i] = U[row];
                U[row] = c;
            }
        }     

        //!!!Найти обраную к исходной
        public void FindA1()
        {
            A1 = Multiplication(Reverse(U), Reverse(L));
        }

        //Нахождение транспонированной матрицы
        public double[,] Tran(double[,] Mas)
        {
            double[,] C = Mas;
            for (int i=0;i<n;i++)
                for (int j=0;j<i;j++)
                    if (i!=j)
                    {
                        double c = C[i, j];
                        C[i, j] = C[j, i];
                        C[j, i] = c;
                    }
            return C;
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
            return det*kol;
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

        //Нахождение обратной матрицы
        public double[,] Reverse(double[,] Mas )
        {
            double o = Determ(Mas);
            double[,] C = new double[n,n];

            for (int i = 0; i < n; i++)
                for (int j = 0; j < n; j++)
                    C[i, j] =Mas[i, j];

            C = Tran(AlgAdd(C));
            for (int i = 0; i < n; i++)
                for (int j = 0; j < n; j++)
                    C[i, j] = C[i, j] / o;
            return C;
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
                sum += A[k] * B[k,i];
            return sum;
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

        //Умножение вектора на матрицу
        public double[] Multiplication(double[] A, double[,] B)
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

        
        //!!!Решение системы уравнений
        public void Solve()
        {
            //solution = 
        }
    }
}
