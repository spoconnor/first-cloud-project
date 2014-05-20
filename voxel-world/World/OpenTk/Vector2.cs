using System;
namespace OpenTK
{
    public struct Vector2 : IEquatable<Vector2>
    {
        //
        // Static Fields
        //
        
        public static readonly int SizeInBytes;
        
        public static readonly Vector2 UnitX;
        
        public static readonly Vector2 UnitY;
        
        public static readonly Vector2 Zero;
        
        public static readonly Vector2 One;
        
        //
        // Fields
        //
        
        public float X;
        
        public float Y;
        
        //
        // Properties
        //
        
        public float Length
        {
            get
            {
                return (float)Math.Sqrt ((double)(this.X * this.X + this.Y * this.Y));
            }
        }
        
        public float LengthFast
        {
            get
            {
                return 1f / MathHelper.InverseSqrtFast (this.X * this.X + this.Y * this.Y);
            }
        }
        
        public float LengthSquared
        {
            get
            {
                return this.X * this.X + this.Y * this.Y;
            }
        }
        
        public Vector2 PerpendicularLeft
        {
            get
            {
                return new Vector2 (-this.Y, this.X);
            }
        }
        
        public Vector2 PerpendicularRight
        {
            get
            {
                return new Vector2 (this.Y, -this.X);
            }
        }
        
        //
        // Constructors
        //
        
        [Obsolete]
        public Vector2 (Vector4 v)
        {
            this.X = v.X;
            this.Y = v.Y;
        }
        
        [Obsolete]
        public Vector2 (Vector3 v)
        {
            this.X = v.X;
            this.Y = v.Y;
        }
        
        [Obsolete]
        public Vector2 (Vector2 v)
        {
            this.X = v.X;
            this.Y = v.Y;
        }
        
        public Vector2 (float x, float y)
        {
            this.X = x;
            this.Y = y;
        }
        
        //
        // Static Methods
        //
        
        public static void Add (ref Vector2 a, ref Vector2 b, out Vector2 result)
        {
            result = new Vector2 (a.X + b.X, a.Y + b.Y);
        }
        
        public static Vector2 Add (Vector2 a, Vector2 b)
        {
            Vector2.Add (ref a, ref b, out a);
            return a;
        }
        
        public static Vector2 BaryCentric (Vector2 a, Vector2 b, Vector2 c, float u, float v)
        {
            return a + u * (b - a) + v * (c - a);
        }
        
        public static void BaryCentric (ref Vector2 a, ref Vector2 b, ref Vector2 c, float u, float v, out Vector2 result)
        {
            result = a;
            Vector2 vector = b;
            Vector2.Subtract (ref vector, ref a, out vector);
            Vector2.Multiply (ref vector, u, out vector);
            Vector2.Add (ref result, ref vector, out result);
            vector = c;
            Vector2.Subtract (ref vector, ref a, out vector);
            Vector2.Multiply (ref vector, v, out vector);
            Vector2.Add (ref result, ref vector, out result);
        }
        
        public static Vector2 Clamp (Vector2 vec, Vector2 min, Vector2 max)
        {
            vec.X = ((vec.X < min.X) ? min.X : ((vec.X > max.X) ? max.X : vec.X));
            vec.Y = ((vec.Y < min.Y) ? min.Y : ((vec.Y > max.Y) ? max.Y : vec.Y));
            return vec;
        }
        
        public static void Clamp (ref Vector2 vec, ref Vector2 min, ref Vector2 max, out Vector2 result)
        {
            result.X = ((vec.X < min.X) ? min.X : ((vec.X > max.X) ? max.X : vec.X));
            result.Y = ((vec.Y < min.Y) ? min.Y : ((vec.Y > max.Y) ? max.Y : vec.Y));
        }
        
        public static Vector2 ComponentMax (Vector2 a, Vector2 b)
        {
            a.X = ((a.X > b.X) ? a.X : b.X);
            a.Y = ((a.Y > b.Y) ? a.Y : b.Y);
            return a;
        }
        
        public static void ComponentMax (ref Vector2 a, ref Vector2 b, out Vector2 result)
        {
            result.X = ((a.X > b.X) ? a.X : b.X);
            result.Y = ((a.Y > b.Y) ? a.Y : b.Y);
        }
        
        public static Vector2 ComponentMin (Vector2 a, Vector2 b)
        {
            a.X = ((a.X < b.X) ? a.X : b.X);
            a.Y = ((a.Y < b.Y) ? a.Y : b.Y);
            return a;
        }
        
        public static void ComponentMin (ref Vector2 a, ref Vector2 b, out Vector2 result)
        {
            result.X = ((a.X < b.X) ? a.X : b.X);
            result.Y = ((a.Y < b.Y) ? a.Y : b.Y);
        }
        
        [Obsolete ("Use static Divide() method instead.")]
        public static void Div (ref Vector2 a, float f, out Vector2 result)
        {
            float num = 1f / f;
            result.X = a.X * num;
            result.Y = a.Y * num;
        }
        
        [Obsolete ("Use static Divide() method instead.")]
        public static Vector2 Div (Vector2 a, float f)
        {
            float num = 1f / f;
            a.X *= num;
            a.Y *= num;
            return a;
        }
        
        public static Vector2 Divide (Vector2 vector, Vector2 scale)
        {
            Vector2.Divide (ref vector, ref scale, out vector);
            return vector;
        }
        
        public static Vector2 Divide (Vector2 vector, float scale)
        {
            Vector2.Divide (ref vector, scale, out vector);
            return vector;
        }
        
        public static void Divide (ref Vector2 vector, float scale, out Vector2 result)
        {
            Vector2.Multiply (ref vector, 1f / scale, out result);
        }
        
        public static void Divide (ref Vector2 vector, ref Vector2 scale, out Vector2 result)
        {
            result = new Vector2 (vector.X / scale.X, vector.Y / scale.Y);
        }
        
        public static void Dot (ref Vector2 left, ref Vector2 right, out float result)
        {
            result = left.X * right.X + left.Y * right.Y;
        }
        
        public static float Dot (Vector2 left, Vector2 right)
        {
            return left.X * right.X + left.Y * right.Y;
        }
        
        public static void Lerp (ref Vector2 a, ref Vector2 b, float blend, out Vector2 result)
        {
            result.X = blend * (b.X - a.X) + a.X;
            result.Y = blend * (b.Y - a.Y) + a.Y;
        }
        
        public static Vector2 Lerp (Vector2 a, Vector2 b, float blend)
        {
            a.X = blend * (b.X - a.X) + a.X;
            a.Y = blend * (b.Y - a.Y) + a.Y;
            return a;
        }
        
        public static Vector2 Max (Vector2 left, Vector2 right)
        {
            if (left.LengthSquared < right.LengthSquared)
            {
                return right;
            }
            return left;
        }
        
        public static Vector2 Min (Vector2 left, Vector2 right)
        {
            if (left.LengthSquared >= right.LengthSquared)
            {
                return right;
            }
            return left;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public static Vector2 Mult (Vector2 a, float f)
        {
            a.X *= f;
            a.Y *= f;
            return a;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public static void Mult (ref Vector2 a, float f, out Vector2 result)
        {
            result.X = a.X * f;
            result.Y = a.Y * f;
        }
        
        public static void Multiply (ref Vector2 vector, ref Vector2 scale, out Vector2 result)
        {
            result = new Vector2 (vector.X * scale.X, vector.Y * scale.Y);
        }
        
        public static Vector2 Multiply (Vector2 vector, float scale)
        {
            Vector2.Multiply (ref vector, scale, out vector);
            return vector;
        }
        
        public static void Multiply (ref Vector2 vector, float scale, out Vector2 result)
        {
            result = new Vector2 (vector.X * scale, vector.Y * scale);
        }
        
        public static Vector2 Multiply (Vector2 vector, Vector2 scale)
        {
            Vector2.Multiply (ref vector, ref scale, out vector);
            return vector;
        }
        
        public static Vector2 Normalize (Vector2 vec)
        {
            float num = 1f / vec.Length;
            vec.X *= num;
            vec.Y *= num;
            return vec;
        }
        
        public static void Normalize (ref Vector2 vec, out Vector2 result)
        {
            float num = 1f / vec.Length;
            result.X = vec.X * num;
            result.Y = vec.Y * num;
        }
        
        public static Vector2 NormalizeFast (Vector2 vec)
        {
            float num = MathHelper.InverseSqrtFast (vec.X * vec.X + vec.Y * vec.Y);
            vec.X *= num;
            vec.Y *= num;
            return vec;
        }
        
        public static void NormalizeFast (ref Vector2 vec, out Vector2 result)
        {
            float num = MathHelper.InverseSqrtFast (vec.X * vec.X + vec.Y * vec.Y);
            result.X = vec.X * num;
            result.Y = vec.Y * num;
        }
        
        [Obsolete ("Use static Subtract() method instead.")]
        public static Vector2 Sub (Vector2 a, Vector2 b)
        {
            a.X -= b.X;
            a.Y -= b.Y;
            return a;
        }
        
        [Obsolete ("Use static Subtract() method instead.")]
        public static void Sub (ref Vector2 a, ref Vector2 b, out Vector2 result)
        {
            result.X = a.X - b.X;
            result.Y = a.Y - b.Y;
        }
        
        public static Vector2 Subtract (Vector2 a, Vector2 b)
        {
            Vector2.Subtract (ref a, ref b, out a);
            return a;
        }
        
        public static void Subtract (ref Vector2 a, ref Vector2 b, out Vector2 result)
        {
            result = new Vector2 (a.X - b.X, a.Y - b.Y);
        }
        
        public static Vector2 Transform (Vector2 vec, Quaternion quat)
        {
            Vector2 result;
            Vector2.Transform (ref vec, ref quat, out result);
            return result;
        }
        
        public static void Transform (ref Vector2 vec, ref Quaternion quat, out Vector2 result)
        {
            Quaternion quaternion = new Quaternion (vec.X, vec.Y, 0f, 0f);
            Quaternion quaternion2;
            Quaternion.Invert (ref quat, out quaternion2);
            Quaternion quaternion3;
            Quaternion.Multiply (ref quat, ref quaternion, out quaternion3);
            Quaternion.Multiply (ref quaternion3, ref quaternion2, out quaternion);
            result = new Vector2 (quaternion.X, quaternion.Y);
        }
        
        //
        // Methods
        //
        
        [CLSCompliant (false), Obsolete ("Use static Add() method instead.")]
        public void Add (ref Vector2 right)
        {
            this.X += right.X;
            this.Y += right.Y;
        }
        
        [Obsolete ("Use static Add() method instead.")]
        public void Add (Vector2 right)
        {
            this.X += right.X;
            this.Y += right.Y;
        }
        
        [Obsolete ("Use static Divide() method instead.")]
        public void Div (float f)
        {
            float num = 1f / f;
            this.X *= num;
            this.Y *= num;
        }
        
        public override bool Equals (object obj)
        {
            return obj is Vector2 && this.Equals ((Vector2)obj);
        }
        
        public bool Equals (Vector2 other)
        {
            return this.X == other.X && this.Y == other.Y;
        }
        
        public override int GetHashCode ()
        {
            return this.X.GetHashCode () ^ this.Y.GetHashCode ();
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Mult (float f)
        {
            this.X *= f;
            this.Y *= f;
        }
        
        public void Normalize ()
        {
            float num = 1f / this.Length;
            this.X *= num;
            this.Y *= num;
        }
        
        public void NormalizeFast ()
        {
            float num = MathHelper.InverseSqrtFast (this.X * this.X + this.Y * this.Y);
            this.X *= num;
            this.Y *= num;
        }
        
        [CLSCompliant (false), Obsolete ("Use static Multiply() method instead.")]
        public void Scale (ref Vector2 scale)
        {
            this.X *= scale.X;
            this.Y *= scale.Y;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Scale (Vector2 scale)
        {
            this.X *= scale.X;
            this.Y *= scale.Y;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Scale (float sx, float sy)
        {
            this.X *= sx;
            this.Y *= sy;
        }
        
        [CLSCompliant (false), Obsolete ("Use static Subtract() method instead.")]
        public void Sub (ref Vector2 right)
        {
            this.X -= right.X;
            this.Y -= right.Y;
        }
        
        [Obsolete ("Use static Subtract() method instead.")]
        public void Sub (Vector2 right)
        {
            this.X -= right.X;
            this.Y -= right.Y;
        }
        
        public override string ToString ()
        {
            return string.Format ("({0}, {1})", this.X, this.Y);
        }
        
        //
        // Operators
        //
        
        public static Vector2 operator + (Vector2 left, Vector2 right)
        {
            left.X += right.X;
            left.Y += right.Y;
            return left;
        }
        
        public static Vector2 operator / (Vector2 vec, float scale)
        {
            float num = 1f / scale;
            vec.X *= num;
            vec.Y *= num;
            return vec;
        }
        
        public static bool operator == (Vector2 left, Vector2 right)
        {
            return left.Equals (right);
        }
        
        public static bool operator != (Vector2 left, Vector2 right)
        {
            return !left.Equals (right);
        }
        
        public static Vector2 operator * (float scale, Vector2 vec)
        {
            vec.X *= scale;
            vec.Y *= scale;
            return vec;
        }
        
        public static Vector2 operator * (Vector2 vec, float scale)
        {
            vec.X *= scale;
            vec.Y *= scale;
            return vec;
        }
        
        public static Vector2 operator - (Vector2 left, Vector2 right)
        {
            left.X -= right.X;
            left.Y -= right.Y;
            return left;
        }
        
        public static Vector2 operator - (Vector2 vec)
        {
            vec.X = -vec.X;
            vec.Y = -vec.Y;
            return vec;
        }
    }
}
