using System;
using System.Xml.Serialization;
namespace OpenTK
{
    public struct Vector4 : IEquatable<Vector4>
    {
        //
        // Static Fields
        //
        
        public static readonly Vector4 One;
        
        public static Vector4 UnitZ;
        
        public static Vector4 UnitY;
        
        public static Vector4 UnitX;
        
        public static Vector4 Zero;
        
        public static readonly int SizeInBytes;
        
        public static Vector4 UnitW;
        
        //
        // Fields
        //
        
        public float X;
        
        public float W;
        
        public float Z;
        
        public float Y;
        
        //
        // Properties
        //
        
        public float Length
        {
            get
            {
                return (float)Math.Sqrt ((double)(this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W));
            }
        }
        
        public float LengthFast
        {
            get
            {
                return 1f / MathHelper.InverseSqrtFast (this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W);
            }
        }
        
        public float LengthSquared
        {
            get
            {
                return this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W;
            }
        }
        
        [XmlIgnore]
        public Vector2 Xy
        {
            get
            {
                return new Vector2 (this.X, this.Y);
            }
            set
            {
                this.X = value.X;
                this.Y = value.Y;
            }
        }
        
        [XmlIgnore]
        public Vector3 Xyz
        {
            get
            {
                return new Vector3 (this.X, this.Y, this.Z);
            }
            set
            {
                this.X = value.X;
                this.Y = value.Y;
                this.Z = value.Z;
            }
        }
        
        //
        // Constructors
        //
        
        public Vector4 (Vector4 v)
        {
            this.X = v.X;
            this.Y = v.Y;
            this.Z = v.Z;
            this.W = v.W;
        }
        
        public Vector4 (Vector3 v, float w)
        {
            this.X = v.X;
            this.Y = v.Y;
            this.Z = v.Z;
            this.W = w;
        }
        
        public Vector4 (Vector3 v)
        {
            this.X = v.X;
            this.Y = v.Y;
            this.Z = v.Z;
            this.W = 0f;
        }
        
        public Vector4 (Vector2 v)
        {
            this.X = v.X;
            this.Y = v.Y;
            this.Z = 0f;
            this.W = 0f;
        }
        
        public Vector4 (float x, float y, float z, float w)
        {
            this.X = x;
            this.Y = y;
            this.Z = z;
            this.W = w;
        }
        
        //
        // Static Methods
        //
        
        public static Vector4 Add (Vector4 a, Vector4 b)
        {
            Vector4.Add (ref a, ref b, out a);
            return a;
        }
        
        public static void Add (ref Vector4 a, ref Vector4 b, out Vector4 result)
        {
            result = new Vector4 (a.X + b.X, a.Y + b.Y, a.Z + b.Z, a.W + b.W);
        }
        
        public static Vector4 BaryCentric (Vector4 a, Vector4 b, Vector4 c, float u, float v)
        {
            return a + u * (b - a) + v * (c - a);
        }
        
        public static void BaryCentric (ref Vector4 a, ref Vector4 b, ref Vector4 c, float u, float v, out Vector4 result)
        {
            result = a;
            Vector4 vector = b;
            Vector4.Subtract (ref vector, ref a, out vector);
            Vector4.Multiply (ref vector, u, out vector);
            Vector4.Add (ref result, ref vector, out result);
            vector = c;
            Vector4.Subtract (ref vector, ref a, out vector);
            Vector4.Multiply (ref vector, v, out vector);
            Vector4.Add (ref result, ref vector, out result);
        }
        
        public static void Clamp (ref Vector4 vec, ref Vector4 min, ref Vector4 max, out Vector4 result)
        {
            result.X = ((vec.X < min.X) ? min.X : ((vec.X > max.X) ? max.X : vec.X));
            result.Y = ((vec.Y < min.Y) ? min.Y : ((vec.Y > max.Y) ? max.Y : vec.Y));
            result.Z = ((vec.X < min.Z) ? min.Z : ((vec.Z > max.Z) ? max.Z : vec.Z));
            result.W = ((vec.Y < min.W) ? min.W : ((vec.W > max.W) ? max.W : vec.W));
        }
        
        public static Vector4 Clamp (Vector4 vec, Vector4 min, Vector4 max)
        {
            vec.X = ((vec.X < min.X) ? min.X : ((vec.X > max.X) ? max.X : vec.X));
            vec.Y = ((vec.Y < min.Y) ? min.Y : ((vec.Y > max.Y) ? max.Y : vec.Y));
            vec.Z = ((vec.X < min.Z) ? min.Z : ((vec.Z > max.Z) ? max.Z : vec.Z));
            vec.W = ((vec.Y < min.W) ? min.W : ((vec.W > max.W) ? max.W : vec.W));
            return vec;
        }
        
        public static void Div (ref Vector4 a, float f, out Vector4 result)
        {
            float num = 1f / f;
            result.X = a.X * num;
            result.Y = a.Y * num;
            result.Z = a.Z * num;
            result.W = a.W * num;
        }
        
        public static Vector4 Div (Vector4 a, float f)
        {
            float num = 1f / f;
            a.X *= num;
            a.Y *= num;
            a.Z *= num;
            a.W *= num;
            return a;
        }
        
        public static Vector4 Divide (Vector4 vector, float scale)
        {
            Vector4.Divide (ref vector, scale, out vector);
            return vector;
        }
        
        public static void Divide (ref Vector4 vector, float scale, out Vector4 result)
        {
            Vector4.Multiply (ref vector, 1f / scale, out result);
        }
        
        public static Vector4 Divide (Vector4 vector, Vector4 scale)
        {
            Vector4.Divide (ref vector, ref scale, out vector);
            return vector;
        }
        
        public static void Divide (ref Vector4 vector, ref Vector4 scale, out Vector4 result)
        {
            result = new Vector4 (vector.X / scale.X, vector.Y / scale.Y, vector.Z / scale.Z, vector.W / scale.W);
        }
        
        public static float Dot (Vector4 left, Vector4 right)
        {
            return left.X * right.X + left.Y * right.Y + left.Z * right.Z + left.W * right.W;
        }
        
        public static void Dot (ref Vector4 left, ref Vector4 right, out float result)
        {
            result = left.X * right.X + left.Y * right.Y + left.Z * right.Z + left.W * right.W;
        }
        
        public static void Lerp (ref Vector4 a, ref Vector4 b, float blend, out Vector4 result)
        {
            result.X = blend * (b.X - a.X) + a.X;
            result.Y = blend * (b.Y - a.Y) + a.Y;
            result.Z = blend * (b.Z - a.Z) + a.Z;
            result.W = blend * (b.W - a.W) + a.W;
        }
        
        public static Vector4 Lerp (Vector4 a, Vector4 b, float blend)
        {
            a.X = blend * (b.X - a.X) + a.X;
            a.Y = blend * (b.Y - a.Y) + a.Y;
            a.Z = blend * (b.Z - a.Z) + a.Z;
            a.W = blend * (b.W - a.W) + a.W;
            return a;
        }
        
        public static void Max (ref Vector4 a, ref Vector4 b, out Vector4 result)
        {
            result.X = ((a.X > b.X) ? a.X : b.X);
            result.Y = ((a.Y > b.Y) ? a.Y : b.Y);
            result.Z = ((a.Z > b.Z) ? a.Z : b.Z);
            result.W = ((a.W > b.W) ? a.W : b.W);
        }
        
        public static Vector4 Max (Vector4 a, Vector4 b)
        {
            a.X = ((a.X > b.X) ? a.X : b.X);
            a.Y = ((a.Y > b.Y) ? a.Y : b.Y);
            a.Z = ((a.Z > b.Z) ? a.Z : b.Z);
            a.W = ((a.W > b.W) ? a.W : b.W);
            return a;
        }
        
        public static Vector4 Min (Vector4 a, Vector4 b)
        {
            a.X = ((a.X < b.X) ? a.X : b.X);
            a.Y = ((a.Y < b.Y) ? a.Y : b.Y);
            a.Z = ((a.Z < b.Z) ? a.Z : b.Z);
            a.W = ((a.W < b.W) ? a.W : b.W);
            return a;
        }
        
        public static void Min (ref Vector4 a, ref Vector4 b, out Vector4 result)
        {
            result.X = ((a.X < b.X) ? a.X : b.X);
            result.Y = ((a.Y < b.Y) ? a.Y : b.Y);
            result.Z = ((a.Z < b.Z) ? a.Z : b.Z);
            result.W = ((a.W < b.W) ? a.W : b.W);
        }
        
        public static void Mult (ref Vector4 a, float f, out Vector4 result)
        {
            result.X = a.X * f;
            result.Y = a.Y * f;
            result.Z = a.Z * f;
            result.W = a.W * f;
        }
        
        public static Vector4 Mult (Vector4 a, float f)
        {
            a.X *= f;
            a.Y *= f;
            a.Z *= f;
            a.W *= f;
            return a;
        }
        
        public static void Multiply (ref Vector4 vector, ref Vector4 scale, out Vector4 result)
        {
            result = new Vector4 (vector.X * scale.X, vector.Y * scale.Y, vector.Z * scale.Z, vector.W * scale.W);
        }
        
        public static Vector4 Multiply (Vector4 vector, Vector4 scale)
        {
            Vector4.Multiply (ref vector, ref scale, out vector);
            return vector;
        }
        
        public static void Multiply (ref Vector4 vector, float scale, out Vector4 result)
        {
            result = new Vector4 (vector.X * scale, vector.Y * scale, vector.Z * scale, vector.W * scale);
        }
        
        public static Vector4 Multiply (Vector4 vector, float scale)
        {
            Vector4.Multiply (ref vector, scale, out vector);
            return vector;
        }
        
        public static Vector4 Normalize (Vector4 vec)
        {
            float num = 1f / vec.Length;
            vec.X *= num;
            vec.Y *= num;
            vec.Z *= num;
            vec.W *= num;
            return vec;
        }
        
        public static void Normalize (ref Vector4 vec, out Vector4 result)
        {
            float num = 1f / vec.Length;
            result.X = vec.X * num;
            result.Y = vec.Y * num;
            result.Z = vec.Z * num;
            result.W = vec.W * num;
        }
        
        public static Vector4 NormalizeFast (Vector4 vec)
        {
            float num = MathHelper.InverseSqrtFast (vec.X * vec.X + vec.Y * vec.Y + vec.Z * vec.Z + vec.W * vec.W);
            vec.X *= num;
            vec.Y *= num;
            vec.Z *= num;
            vec.W *= num;
            return vec;
        }
        
        public static void NormalizeFast (ref Vector4 vec, out Vector4 result)
        {
            float num = MathHelper.InverseSqrtFast (vec.X * vec.X + vec.Y * vec.Y + vec.Z * vec.Z + vec.W * vec.W);
            result.X = vec.X * num;
            result.Y = vec.Y * num;
            result.Z = vec.Z * num;
            result.W = vec.W * num;
        }
        
        public static Vector4 Sub (Vector4 a, Vector4 b)
        {
            a.X -= b.X;
            a.Y -= b.Y;
            a.Z -= b.Z;
            a.W -= b.W;
            return a;
        }
        
        public static void Sub (ref Vector4 a, ref Vector4 b, out Vector4 result)
        {
            result.X = a.X - b.X;
            result.Y = a.Y - b.Y;
            result.Z = a.Z - b.Z;
            result.W = a.W - b.W;
        }
        
        public static void Subtract (ref Vector4 a, ref Vector4 b, out Vector4 result)
        {
            result = new Vector4 (a.X - b.X, a.Y - b.Y, a.Z - b.Z, a.W - b.W);
        }
        
        public static Vector4 Subtract (Vector4 a, Vector4 b)
        {
            Vector4.Subtract (ref a, ref b, out a);
            return a;
        }
        
        public static Vector4 Transform (Vector4 vec, Matrix4 mat)
        {
            Vector4 result;
            Vector4.Transform (ref vec, ref mat, out result);
            return result;
        }
        
        public static void Transform (ref Vector4 vec, ref Matrix4 mat, out Vector4 result)
        {
            result = new Vector4 (vec.X * mat.Row0.X + vec.Y * mat.Row1.X + vec.Z * mat.Row2.X + vec.W * mat.Row3.X, vec.X * mat.Row0.Y + vec.Y * mat.Row1.Y + vec.Z * mat.Row2.Y + vec.W * mat.Row3.Y, vec.X * mat.Row0.Z + vec.Y * mat.Row1.Z + vec.Z * mat.Row2.Z + vec.W * mat.Row3.Z, vec.X * mat.Row0.W + vec.Y * mat.Row1.W + vec.Z * mat.Row2.W + vec.W * mat.Row3.W);
        }
        
        public static Vector4 Transform (Vector4 vec, Quaternion quat)
        {
            Vector4 result;
            Vector4.Transform (ref vec, ref quat, out result);
            return result;
        }
        
        public static void Transform (ref Vector4 vec, ref Quaternion quat, out Vector4 result)
        {
            Quaternion quaternion = new Quaternion (vec.X, vec.Y, vec.Z, vec.W);
            Quaternion quaternion2;
            Quaternion.Invert (ref quat, out quaternion2);
            Quaternion quaternion3;
            Quaternion.Multiply (ref quat, ref quaternion, out quaternion3);
            Quaternion.Multiply (ref quaternion3, ref quaternion2, out quaternion);
            result = new Vector4 (quaternion.X, quaternion.Y, quaternion.Z, quaternion.W);
        }
        
        //
        // Methods
        //
        
        [Obsolete ("Use static Add() method instead.")]
        public void Add (Vector4 right)
        {
            this.X += right.X;
            this.Y += right.Y;
            this.Z += right.Z;
            this.W += right.W;
        }
        
        [CLSCompliant (false), Obsolete ("Use static Add() method instead.")]
        public void Add (ref Vector4 right)
        {
            this.X += right.X;
            this.Y += right.Y;
            this.Z += right.Z;
            this.W += right.W;
        }
        
        [Obsolete ("Use static Divide() method instead.")]
        public void Div (float f)
        {
            float num = 1f / f;
            this.X *= num;
            this.Y *= num;
            this.Z *= num;
            this.W *= num;
        }
        
        public bool Equals (Vector4 other)
        {
            return this.X == other.X && this.Y == other.Y && this.Z == other.Z && this.W == other.W;
        }
        
        public override bool Equals (object obj)
        {
            return obj is Vector4 && this.Equals ((Vector4)obj);
        }
        
        public override int GetHashCode ()
        {
            return this.X.GetHashCode () ^ this.Y.GetHashCode () ^ this.Z.GetHashCode () ^ this.W.GetHashCode ();
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Mult (float f)
        {
            this.X *= f;
            this.Y *= f;
            this.Z *= f;
            this.W *= f;
        }
        
        public void Normalize ()
        {
            float num = 1f / this.Length;
            this.X *= num;
            this.Y *= num;
            this.Z *= num;
            this.W *= num;
        }
        
        public void NormalizeFast ()
        {
            float num = MathHelper.InverseSqrtFast (this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W);
            this.X *= num;
            this.Y *= num;
            this.Z *= num;
            this.W *= num;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Scale (float sx, float sy, float sz, float sw)
        {
            this.X *= sx;
            this.Y *= sy;
            this.Z *= sz;
            this.W *= sw;
        }
        
        [CLSCompliant (false), Obsolete ("Use static Multiply() method instead.")]
        public void Scale (ref Vector4 scale)
        {
            this.X *= scale.X;
            this.Y *= scale.Y;
            this.Z *= scale.Z;
            this.W *= scale.W;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Scale (Vector4 scale)
        {
            this.X *= scale.X;
            this.Y *= scale.Y;
            this.Z *= scale.Z;
            this.W *= scale.W;
        }
        
        [CLSCompliant (false), Obsolete ("Use static Subtract() method instead.")]
        public void Sub (ref Vector4 right)
        {
            this.X -= right.X;
            this.Y -= right.Y;
            this.Z -= right.Z;
            this.W -= right.W;
        }
        
        [Obsolete ("Use static Subtract() method instead.")]
        public void Sub (Vector4 right)
        {
            this.X -= right.X;
            this.Y -= right.Y;
            this.Z -= right.Z;
            this.W -= right.W;
        }
        
        public override string ToString ()
        {
            return string.Format ("({0}, {1}, {2}, {3})", new object[]
                                  {
                this.X,
                this.Y,
                this.Z,
                this.W
            });
        }
        
        //
        // Operators
        //
        
        public static Vector4 operator + (Vector4 left, Vector4 right)
        {
            left.X += right.X;
            left.Y += right.Y;
            left.Z += right.Z;
            left.W += right.W;
            return left;
        }
        
        public static Vector4 operator / (Vector4 vec, float scale)
        {
            float num = 1f / scale;
            vec.X *= num;
            vec.Y *= num;
            vec.Z *= num;
            vec.W *= num;
            return vec;
        }
        
        public static bool operator == (Vector4 left, Vector4 right)
        {
            return left.Equals (right);
        }
        
        public unsafe static explicit operator IntPtr (Vector4 v)
        {
            return (IntPtr)((void*)(&v.X));
        }
        
        [CLSCompliant (false)]
        public unsafe static explicit operator float* (Vector4 v)
        {
            return &v.X;
        }
        
        public static bool operator != (Vector4 left, Vector4 right)
        {
            return !left.Equals (right);
        }
        
        public static Vector4 operator * (float scale, Vector4 vec)
        {
            vec.X *= scale;
            vec.Y *= scale;
            vec.Z *= scale;
            vec.W *= scale;
            return vec;
        }
        
        public static Vector4 operator * (Vector4 vec, float scale)
        {
            vec.X *= scale;
            vec.Y *= scale;
            vec.Z *= scale;
            vec.W *= scale;
            return vec;
        }
        
        public static Vector4 operator - (Vector4 left, Vector4 right)
        {
            left.X -= right.X;
            left.Y -= right.Y;
            left.Z -= right.Z;
            left.W -= right.W;
            return left;
        }
        
        public static Vector4 operator - (Vector4 vec)
        {
            vec.X = -vec.X;
            vec.Y = -vec.Y;
            vec.Z = -vec.Z;
            vec.W = -vec.W;
            return vec;
        }
    }
}
