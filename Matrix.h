#pragma once

#include<bitset>
#include<iostream>
#include<thread>

//#include"Define.h"
#define USE_THREADS 1

template<typename T>
class Matrix
{
private:
	T** _matrix;
	unsigned int _rows;
	unsigned int _cols;

	//TODO with threads
	void alloc(T*** matrix)
	{
		*matrix = new T * [_rows];
		for (unsigned int rows = 0; rows < _rows; (*matrix)[rows] = new T[_cols], rows++);
	}

public:
	
	//TODO - Multithread constructor Matrix<T>(unsigned int rows, unsigned int cols)
	static void CreateMatrix()
	{

	}
	
	static void DotProductThread(_Matrix& res, const int threadNumber, _Matrix& _this, _Matrix& matrix)
	{
		unsigned int nElements = res._cols * res._rows;
		unsigned int nOperations = nElements / THREADS_NUMBER;
		unsigned int restOperations = nElements % THREADS_NUMBER;
		unsigned int startOp, endOp;

		if (threadNumber == 0) {
			startOp = nOperations * threadNumber;
			endOp = (nOperations * (threadNumber + 1)) + restOperations;
		}
		else {
			startOp = nOperations * threadNumber + restOperations;
			endOp = (nOperations * (threadNumber + 1)) + restOperations;
		}

		for (unsigned int op = startOp; op < endOp; ++op) {
			unsigned int row = op % res._cols;
			unsigned int col = op / res._rows;
			res._matrix[row][col] = 0;
			for (unsigned int r = 0; r < res._rows; ++r)
				res._matrix[row][col] += _this._matrix[row][r] * matrix._matrix[r][col];
		}
	}

	//TODO - complete
	static void KroneckerProductThread(_Matrix& res, const int threadNumber, _Matrix& _this, _Matrix& matrix)
	{
		unsigned int nElements = res._cols * res._rows;
		unsigned int nOperations = nElements / THREADS_NUMBER;
		unsigned int restOperations = nElements % THREADS_NUMBER;
		unsigned int startOp, endOp;

		if (threadNumber == 0) {
			startOp = nOperations * threadNumber;
			endOp = (nOperations * (threadNumber + 1)) + restOperations;
		}
		else {
			startOp = nOperations * threadNumber + restOperations;
			endOp = (nOperations * (threadNumber + 1)) + restOperations;
		}

		for (unsigned int op = startOp; op < endOp; ++op) {
			unsigned int row = op % res._cols;
			unsigned int col = op / res._rows;
			for (unsigned int r = 0; r < res._rows; ++r)
				std::cout << row << " " << col << std::endl;
				//res._matrix[row][col] += _this._matrix[row][r] * matrix._matrix[r][col];
		}
		
		//for (unsigned int r = 0; r < res._rows; r++)
		//	for (unsigned int c = 0; c < res._cols; c++)
		//		if ((r + c) % THREADS_NUMBER == threadNumber)
		//			for (unsigned int rM = 0; rM < matrix._rows; rM++)
		//				for (unsigned int cM = 0; cM < matrix._cols; cM++)
		//					res._matrix[r * matrix._rows + rM][c * matrix._cols + cM] = _this._matrix[r][c] * matrix._matrix[rM][cM];
	}

	Matrix<T>(unsigned int rows, unsigned int cols)
	{
		_rows = rows;
		_cols = cols;
		alloc(&_matrix);

#if USE_THREADS
		//TODO
#else
		for (rows = 0; rows < _rows; rows++)
			for (cols = 0; cols < _cols; cols++)
				_matrix[rows][cols] = 0;
#endif
	}

	Matrix<T>(unsigned int rows, unsigned int cols, std::initializer_list<T> args)
	{
		_rows = rows;
		_cols = cols;
		alloc(&_matrix);

		for (rows = 0; rows < _rows; rows++)
			for (cols = 0; cols < _cols; cols++)
				_matrix[rows][cols] = *(args.begin() + (_cols * rows + cols));
	}

	Matrix<T>(unsigned int rows, unsigned int cols, T* args)
	{
		_rows = rows;
		_cols = cols;
		alloc(&_matrix);

		for (rows = 0; rows < _rows; rows++)
			for (cols = 0; cols < _cols; cols++)
				_matrix[rows][cols] = *(args + (_cols * rows + cols));
	}

	inline void print(std::string str = "")
	{
		std::cout << str << std::endl;
		for (unsigned int r = 0; r < _rows; r++)
		{
			for (unsigned int c = 0; c < _cols; c++)
				std::cout << _matrix[r][c] << " ";
			std::cout << std::endl;
		}
		std::cout << "----------" << std::endl;
	}

	//Matrix dot product
	Matrix<T>* DotProduct(Matrix<T>* matrix)
	{
#if USE_THREADS
		_Matrix* res = new _Matrix(_rows, _cols);

		std::thread threads[THREADS_NUMBER];
		for (unsigned int i = 0; i < THREADS_NUMBER; ++i)
			threads[i] = std::thread(DotProductThread, std::ref(*res), i, std::ref(*this), std::ref(*matrix));

		for (unsigned int i = 0; i < THREADS_NUMBER; ++i)
			threads[i].join();

		return res;
#else
		Matrix<T>* res = new Matrix<T>(_rows, _cols);

		for (unsigned int i = 0; i < _rows; i++)
			for (unsigned int j = 0; j < _cols; j++)
			{
				res->_matrix[i][j] = 0;
				for (unsigned int k = 0; k < _rows; k++)
					res->_matrix[i][j] += _matrix[i][k] * matrix->_matrix[k][j];
			}

		return res;
#endif
	}

	//Matrix Kronecker product
	Matrix<T>* KroneckerProduct(Matrix<T>* matrix)
	{
#if USE_THREADS
		_Matrix* res = new _Matrix(_rows * matrix->_rows, _cols * matrix->_cols);

		std::thread threads[THREADS_NUMBER];
		for (unsigned int i = 0; i < THREADS_NUMBER; ++i)
			threads[i] = std::thread(KroneckerProductThread, std::ref(*res), i, std::ref(*this), std::ref(*matrix));

		for (unsigned int i = 0; i < THREADS_NUMBER; ++i)
			threads[i].join();

		return res;
#else
		Matrix<T>* res = new Matrix<T>(_rows * matrix->_rows, _cols * matrix->_cols);

		for (unsigned int r = 0; r < _rows; r++)
			for (unsigned int c = 0; c < _cols; c++)
				for (unsigned int rM = 0; rM < matrix->_rows; rM++)
					for (unsigned int cM = 0; cM < matrix->_cols; cM++)
						res->_matrix[r * matrix->_rows + rM][c * matrix->_cols + cM] = _matrix[r][c] * matrix->_matrix[rM][cM];

		return res;
#endif
	}

	//Matrix Kronecker product given size of matrix
	static Matrix<T>* KroneckerProduct(unsigned int size)
	{
		if (size == 0)
			return new Matrix<T>(1, 1, { 1 });

		Matrix<T>* result = new Matrix<T>(2, 2, { 1, 0, 0, 1 });
		for (; size > 0; size--)
			result = result->KroneckerProduct(new Matrix<T>(2, 2, { 1, 0, 0, 1 }));

		return result;
	}
};

template<typename T>
Matrix<T>* CalculateCXMatrix(unsigned int size, unsigned int check, unsigned int target)
{
	unsigned int qreg = (unsigned int)pow(2, size);
	T* args = new T[(size_t)pow(qreg, 2)];

	for (unsigned int i = 0; i < qreg; i++)
	{
		std::string s = std::bitset< MAX_BIT_SIZE >(i).to_string().substr(MAX_BIT_SIZE - size, size);
		if (s[check] == '1')
		{
			if (s[target] == '0')
				s[target] = '1';
			else if (s[target] == '1')
				s[target] = '0';
		}

		unsigned int l = (unsigned int)s.length();
		unsigned long long value = 0;
		unsigned long long exp = 1;
		for (unsigned int a = l; a != 0; a--)
		{
			if (s[a - 1] == '1')
				value += exp;
			exp *= 2;
		}
		args[value + i * qreg] = 1;
	}

	Matrix<T>* result = new Matrix<T>(qreg, qreg, args);
	return result;
}