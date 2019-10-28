#include<bitset>
#include<iostream>
#include<thread>

#define MAX_BIT_SIZE 256
#define THREADS_NUMBER 256
#define USE_THREADS 1

template<typename T>
class Matrix
{
public:
	//Constructor
	Matrix<T>(unsigned int rows, unsigned int cols)
	{
		_rows = rows;
		_cols = cols;
		
		_matrix = (T*)malloc(_rows * _cols * sizeof(T));
	}

	//Constructor
	Matrix<T>(unsigned int rows, unsigned int cols, std::initializer_list<T> args)
	{
		_rows = rows;
		_cols = cols;
		
		_matrix = (T*)malloc(_rows * _cols * sizeof(T));

		for (rows = 0; rows < _rows; rows++)
			for (cols = 0; cols < _cols; cols++)
				//_matrix[rows][cols] = *(args.begin() + (_cols * rows + cols));
				set(rows, cols, *(args.begin() + (_cols * rows + cols)));
	}

	//Constructor
	Matrix<T>(unsigned int rows, unsigned int cols, T* args)
	{
		_rows = rows;
		_cols = cols;
		
		_matrix = (T*)malloc(_rows * _cols * sizeof(T));

		for (rows = 0; rows < _rows; rows++)
			for (cols = 0; cols < _cols; cols++)
				set(rows, cols, *(args + (_cols * rows + cols)));
	}

	//Destructor
	~Matrix<T>()
	{
		delete[] _matrix;
	}

	inline Matrix<T>* operator+(const Matrix<T>& matrix)
	{
		Matrix<T>* res = new Matrix<T>(_rows, _cols);

		for (unsigned int r = 0; r < _rows; r++)
			for (unsigned int c = 0; c < _cols; c++)
				res->_matrix[r][c] = _matrix[r][c] + matrix._matrix[r][c];

		return res;
	}

	inline Matrix<T>* operator*(const T& scalar)
	{
		Matrix<T>* res = new Matrix<T>(_rows, _cols);

		for (unsigned int r = 0; r < _rows; r++)
			for (unsigned int c = 0; c < _cols; c++)
				res->set(r, c, get(r, c) * scalar);

		return res;
	}

	//Matrix Kronecker product given size of matrix
	inline static Matrix<T>* KroneckerProduct(unsigned int size)
	{
		if (size == 0)
			return new Matrix<T>(1, 1, { 1 });

		Matrix<T>* result = new Matrix<T>(2, 2, { 1, 0, 0, 1 });
		for (; size > 0; size--)
			result = result->KroneckerProduct(new Matrix<T>(2, 2, { 1, 0, 0, 1 }));

		return result;
	}

	inline static void DotProductThread(Matrix<T>& res, const int threadNumber, Matrix<T>& _this, Matrix<T>& matrix)
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
			res.set(row, col, 0);
			for (unsigned int r = 0; r < res._rows; ++r)
				res.set(row, col, res.get(row, col) + _this.get(row, r) * matrix.get(r, col));
		}
	}

	inline static void KroneckerProductThread(Matrix<T>& res, const int threadNumber, Matrix<T>& _this, Matrix<T>& matrix)
	{
		for (unsigned int r = 0; r < _this._rows; r++)
			for (unsigned int c = 0; c < _this._cols; c++)
				if ((r * res._cols + c) % THREADS_NUMBER == threadNumber)
					res._insertMatrix(r * matrix._rows, c * matrix._cols, matrix.operator*(_this.get(r, c)));
	}

	inline void print(std::string str = "")
	{
		std::cout << str << std::endl;
		for (unsigned int r = 0; r < _rows; r++)
		{
			for (unsigned int c = 0; c < _cols; c++)
				std::cout << get(r, c) << " ";
			std::cout << std::endl;
		}
		std::cout << "----------" << std::endl;
	}

	//Matrix dot product
	inline Matrix<T>* DotProduct(Matrix<T>* matrix)
	{
#if USE_THREADS
		Matrix<T>* res = new Matrix<T>(_rows, _cols);

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
	inline Matrix<T>* KroneckerProduct(Matrix<T>* matrix)
	{
#if USE_THREADS
		Matrix<T>* res = new Matrix<T>(_rows * matrix->_rows, _cols * matrix->_cols);

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
				res->_insertMatrix(r * matrix->_rows, c * matrix->_cols, matrix->operator*(_matrix[r][c]));

		return res;
#endif
	}

private:
	T* _matrix;
	unsigned int _rows;
	unsigned int _cols;

	void _insertMatrix(unsigned int rPos, unsigned int cPos, Matrix<T>* matrix)
	{
		unsigned int maxR = matrix->_rows;
		unsigned int maxC = matrix->_cols;
		for (unsigned int r = 0; r < maxR; r++)
			for (unsigned int c = 0; c < maxC; c++)
				set(rPos + r, cPos + c, matrix->get(r, c));
	}

	void set(unsigned int r, unsigned int c, T el)
	{
		_matrix[r * _rows + c] = el;
	}

	T get(unsigned int r, unsigned int c)
	{
		return _matrix[r * _rows + c];
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
