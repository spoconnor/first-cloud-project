
class Array2dInt
{
public:
	Array2dInt(int width, int height) :
		mWidth(width),
		mHeight(height)
	{
		mArray = new int[width][height];
	}
	~Array2dInt()
	{
		delete mArray;
	}
	int Width() { return mWidth; }
	int Height() { return mHeight; }
private:
	int mWidth;
	int mHeight;
	int* mArray[][];
};
