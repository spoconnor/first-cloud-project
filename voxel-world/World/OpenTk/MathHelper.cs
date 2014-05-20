using System;
namespace OpenTK
{
    public static class MathHelper
    {
        //
        // Static Fields
        //
        
        public const float Pi = 3.14159274f;
        
        public const float Log2E = 1.442695f;
        
        public const float Log10E = 0.4342945f;
        
        public const float E = 2.71828175f;
        
        public const float ThreePiOver2 = 4.712389f;
        
        public const float TwoPi = 6.28318548f;
        
        public const float PiOver2 = 1.57079637f;
        
        public const float PiOver3 = 1.04719758f;
        
        public const float PiOver4 = 0.7853982f;
        
        public const float PiOver6 = 0.5235988f;
        
        //
        // Static Methods
        //
        
        public static long BinomialCoefficient (int n, int k)
        {
            return MathHelper.Factorial (n) / (MathHelper.Factorial (k) * MathHelper.Factorial (n - k));
        }
        
        public static float DegreesToRadians (float degrees)
        {
            return degrees * 0.0174532924f;
        }
        
        public static long Factorial (int n)
        {
            long num = 1L;
            while (n > 1)
            {
                num *= (long)n;
                n--;
            }
            return num;
        }
        
        public static double InverseSqrtFast (double x)
        {
            return (double)MathHelper.InverseSqrtFast ((float)x);
        }
        
        public unsafe static float InverseSqrtFast (float x)
        {
            float num = 0.5f * x;
            int num2 = *(int*)(&x);
            num2 = 1597463174 - (num2 >> 1);
            x = *(float*)(&num2);
            x *= 1.5f - num * x * x;
            return x;
        }
        
        public static long NextPowerOfTwo (long n)
        {
            if (n < 0L)
            {
                throw new ArgumentOutOfRangeException ("n", "Must be positive.");
            }
            return (long)Math.Pow (2.0, Math.Ceiling (Math.Log ((double)n, 2.0)));
        }
        
        public static int NextPowerOfTwo (int n)
        {
            if (n < 0)
            {
                throw new ArgumentOutOfRangeException ("n", "Must be positive.");
            }
            return (int)Math.Pow (2.0, Math.Ceiling (Math.Log ((double)n, 2.0)));
        }
        
        public static float NextPowerOfTwo (float n)
        {
            if (n < 0f)
            {
                throw new ArgumentOutOfRangeException ("n", "Must be positive.");
            }
            return (float)Math.Pow (2.0, Math.Ceiling (Math.Log ((double)n, 2.0)));
        }
        
        public static double NextPowerOfTwo (double n)
        {
            if (n < 0.0)
            {
                throw new ArgumentOutOfRangeException ("n", "Must be positive.");
            }
            return Math.Pow (2.0, Math.Ceiling (Math.Log (n, 2.0)));
        }
        
        public static float RadiansToDegrees (float radians)
        {
            return radians * 57.2957764f;
        }
        
        public static void Swap (ref double a, ref double b)
        {
            double num = a;
            a = b;
            b = num;
        }
        
        public static void Swap (ref float a, ref float b)
        {
            float num = a;
            a = b;
            b = num;
        }
    }
}
