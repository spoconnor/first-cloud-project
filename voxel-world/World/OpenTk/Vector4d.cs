using System;
using System.Xml.Serialization;
namespace OpenTK
{
    public struct Vector4d : IEquatable<Vector4d>
    {
        //
        // Static Fields
        //
        
        public static Vector4d UnitY;
        
        public static readonly int SizeInBytes;
        
        public static readonly Vector4d One;
        
        public static Vector4d Zero;
        
        public static Vector4d UnitX;
        
        public static Vector4d UnitW;
        
        public static Vector4d UnitZ;
        
        //
        // Fields
        //
        
        public double X;
        
        public double W;
        
        public double Z;
        
        public double Y;
        
        //
        // Properties
        //
        
        public double Length
        {
            get
            {
                return Math.Sqrt (this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W);
            }
        }
        
        public double LengthFast
        {
            get
            {
                return 1.0 / MathHelper.InverseSqrtFast (this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W);
            }
        }
        
        public double LengthSquared
        {
            get
            {
                return this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W;
            }
        }

        public Vector2d Xy
        {
            get
            {
                return new Vector2d (this.X, this.Y);
            }
            set
            {
                this.X = value.X;
                this.Y = value.Y;
            }
        }
        
        [XmlIgnore]
        public Vector3d Xyz
        {
            get
            {
                return new Vector3d (this.X, this.Y, this.Z);
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
        
        public Vector4d (Vector4d v)
        {
            this.X = v.X;
            this.Y = v.Y;
            this.Z = v.Z;
            this.W = v.W;
        }
        
        public Vector4d (Vector3d v, double w)
        {
            this.X = v.X;
            this.Y = v.Y;
            this.Z = v.Z;
            this.W = w;
        }
        
        public Vector4d (Vector3d v)
        {
            this.X = v.X;
            this.Y = v.Y;
            this.Z = v.Z;
            this.W = 0.0;
        }
        
        public Vector4d (Vector2d v)
        {
            this.X = v.X;
            this.Y = v.Y;
            this.Z = 0.0;
            this.W = 0.0;
        }
        
        public Vector4d (double x, double y, double z, double w)
        {
            this.X = x;
            this.Y = y;
            this.Z = z;
            this.W = w;
        }
        
        //
        // Static Methods
        //
        
        public static void Add (ref Vector4d a, ref Vector4d b, out Vector4d result)
        {
            result = new Vector4d (a.X + b.X, a.Y + b.Y, a.Z + b.Z, a.W + b.W);
        }
        
        public static Vector4d Add (Vector4d a, Vector4d b)
        {
            Vector4d.Add (ref a, ref b, out a);
            return a;
        }
        
        public static Vector4d BaryCentric (Vector4d a, Vector4d b, Vector4d c, double u, double v)
        {
            return a + u * (b - a) + v * (c - a);
        }
        
        public static void BaryCentric (ref Vector4d a, ref Vector4d b, ref Vector4d c, double u, double v, out Vector4d result)
        {
            result = a;
            Vector4d vector4d = b;
            Vector4d.Subtract (ref vector4d, ref a, out vector4d);
            Vector4d.Multiply (ref vector4d, u, out vector4d);
            Vector4d.Add (ref result, ref vector4d, out result);
            vector4d = c;
            Vector4d.Subtract (ref vector4d, ref a, out vector4d);
            Vector4d.Multiply (ref vector4d, v, out vector4d);
            Vector4d.Add (ref result, ref vector4d, out result);
        }
        
        public static void Clamp (ref Vector4d vec, ref Vector4d min, ref Vector4d max, out Vector4d result)
        {
            result.X = ((vec.X < min.X) ? min.X : ((vec.X > max.X) ? max.X : vec.X));
            result.Y = ((vec.Y < min.Y) ? min.Y : ((vec.Y > max.Y) ? max.Y : vec.Y));
            result.Z = ((vec.X < min.Z) ? min.Z : ((vec.Z > max.Z) ? max.Z : vec.Z));
            result.W = ((vec.Y < min.W) ? min.W : ((vec.W > max.W) ? max.W : vec.W));
        }
        
        public static Vector4d Clamp (Vector4d vec, Vector4d min, Vector4d max)
        {
            vec.X = ((vec.X < min.X) ? min.X : ((vec.X > max.X) ? max.X : vec.X));
            vec.Y = ((vec.Y < min.Y) ? min.Y : ((vec.Y > max.Y) ? max.Y : vec.Y));
            vec.Z = ((vec.X < min.Z) ? min.Z : ((vec.Z > max.Z) ? max.Z : vec.Z));
            vec.W = ((vec.Y < min.W) ? min.W : ((vec.W > max.W) ? max.W : vec.W));
            return vec;
        }
        
        [Obsolete ("Use static Divide() method instead.")]
        public static Vector4d Div (Vector4d a, double f)
        {
            double num = 1.0 / f;
            a.X *= num;
            a.Y *= num;
            a.Z *= num;
            a.W *= num;
            return a;
        }
        
        [Obsolete ("Use static Divide() method instead.")]
        public static void Div (ref Vector4d a, double f, out Vector4d result)
        {
            double num = 1.0 / f;
            result.X = a.X * num;
            result.Y = a.Y * num;
            result.Z = a.Z * num;
            result.W = a.W * num;
        }
        
        public static void Divide (ref Vector4d vector, ref Vector4d scale, out Vector4d result)
        {
            result = new Vector4d (vector.X / scale.X, vector.Y / scale.Y, vector.Z / scale.Z, vector.W / scale.W);
        }
        
        public static Vector4d Divide (Vector4d vector, Vector4d scale)
        {
            Vector4d.Divide (ref vector, ref scale, out vector);
            return vector;
        }
        
        public static void Divide (ref Vector4d vector, double scale, out Vector4d result)
        {
            Vector4d.Multiply (ref vector, 1.0 / scale, out result);
        }
        
        public static Vector4d Divide (Vector4d vector, double scale)
        {
            Vector4d.Divide (ref vector, scale, out vector);
            return vector;
        }
        
        public static double Dot (Vector4d left, Vector4d right)
        {
            return left.X * right.X + left.Y * right.Y + left.Z * right.Z + left.W * right.W;
        }
        
        public static void Dot (ref Vector4d left, ref Vector4d right, out double result)
        {
            result = left.X * right.X + left.Y * right.Y + left.Z * right.Z + left.W * right.W;
        }
        
        public static Vector4d Lerp (Vector4d a, Vector4d b, double blend)
        {
            a.X = blend * (b.X - a.X) + a.X;
            a.Y = blend * (b.Y - a.Y) + a.Y;
            a.Z = blend * (b.Z - a.Z) + a.Z;
            a.W = blend * (b.W - a.W) + a.W;
            return a;
        }
        
        public static void Lerp (ref Vector4d a, ref Vector4d b, double blend, out Vector4d result)
        {
            result.X = blend * (b.X - a.X) + a.X;
            result.Y = blend * (b.Y - a.Y) + a.Y;
            result.Z = blend * (b.Z - a.Z) + a.Z;
            result.W = blend * (b.W - a.W) + a.W;
        }
        
        public static void Max (ref Vector4d a, ref Vector4d b, out Vector4d result)
        {
            result.X = ((a.X > b.X) ? a.X : b.X);
            result.Y = ((a.Y > b.Y) ? a.Y : b.Y);
            result.Z = ((a.Z > b.Z) ? a.Z : b.Z);
            result.W = ((a.W > b.W) ? a.W : b.W);
        }
        
        public static Vector4d Max (Vector4d a, Vector4d b)
        {
            a.X = ((a.X > b.X) ? a.X : b.X);
            a.Y = ((a.Y > b.Y) ? a.Y : b.Y);
            a.Z = ((a.Z > b.Z) ? a.Z : b.Z);
            a.W = ((a.W > b.W) ? a.W : b.W);
            return a;
        }
        
        public static void Min (ref Vector4d a, ref Vector4d b, out Vector4d result)
        {
            result.X = ((a.X < b.X) ? a.X : b.X);
            result.Y = ((a.Y < b.Y) ? a.Y : b.Y);
            result.Z = ((a.Z < b.Z) ? a.Z : b.Z);
            result.W = ((a.W < b.W) ? a.W : b.W);
        }
        
        public static Vector4d Min (Vector4d a, Vector4d b)
        {
            a.X = ((a.X < b.X) ? a.X : b.X);
            a.Y = ((a.Y < b.Y) ? a.Y : b.Y);
            a.Z = ((a.Z < b.Z) ? a.Z : b.Z);
            a.W = ((a.W < b.W) ? a.W : b.W);
            return a;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public static void Mult (ref Vector4d a, double f, out Vector4d result)
        {
            result.X = a.X * f;
            result.Y = a.Y * f;
            result.Z = a.Z * f;
            result.W = a.W * f;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public static Vector4d Mult (Vector4d a, double f)
        {
            a.X *= f;
            a.Y *= f;
            a.Z *= f;
            a.W *= f;
            return a;
        }
        
        public static void Multiply (ref Vector4d vector, ref Vector4d scale, out Vector4d result)
        {
            result = new Vector4d (vector.X * scale.X, vector.Y * scale.Y, vector.Z * scale.Z, vector.W * scale.W);
        }
        
        public static Vector4d Multiply (Vector4d vector, Vector4d scale)
        {
            Vector4d.Multiply (ref vector, ref scale, out vector);
            return vector;
        }
        
        public static void Multiply (ref Vector4d vector, double scale, out Vector4d result)
        {
            result = new Vector4d (vector.X * scale, vector.Y * scale, vector.Z * scale, vector.W * scale);
        }
        
        public static Vector4d Multiply (Vector4d vector, double scale)
        {
            Vector4d.Multiply (ref vector, scale, out vector);
            return vector;
        }
        
        public static Vector4d Normalize (Vector4d vec)
        {
            double num = 1.0 / vec.Length;
            vec.X *= num;
            vec.Y *= num;
            vec.Z *= num;
            vec.W *= num;
            return vec;
        }
        
        public static void Normalize (ref Vector4d vec, out Vector4d result)
        {
            double num = 1.0 / vec.Length;
            result.X = vec.X * num;
            result.Y = vec.Y * num;
            result.Z = vec.Z * num;
            result.W = vec.W * num;
        }
        
        public static Vector4d NormalizeFast (Vector4d vec)
        {
            double num = MathHelper.InverseSqrtFast (vec.X * vec.X + vec.Y * vec.Y + vec.Z * vec.Z + vec.W * vec.W);
            vec.X *= num;
            vec.Y *= num;
            vec.Z *= num;
            vec.W *= num;
            return vec;
        }
        
        public static void NormalizeFast (ref Vector4d vec, out Vector4d result)
        {
            double num = MathHelper.InverseSqrtFast (vec.X * vec.X + vec.Y * vec.Y + vec.Z * vec.Z + vec.W * vec.W);
            result.X = vec.X * num;
            result.Y = vec.Y * num;
            result.Z = vec.Z * num;
            result.W = vec.W * num;
        }
        
        [Obsolete ("Use static Subtract() method instead.")]
        public static Vector4d Sub (Vector4d a, Vector4d b)
        {
            a.X -= b.X;
            a.Y -= b.Y;
            a.Z -= b.Z;
            a.W -= b.W;
            return a;
        }
        
        [Obsolete ("Use static Subtract() method instead.")]
        public static void Sub (ref Vector4d a, ref Vector4d b, out Vector4d result)
        {
            result.X = a.X - b.X;
            result.Y = a.Y - b.Y;
            result.Z = a.Z - b.Z;
            result.W = a.W - b.W;
        }
        
        public static Vector4d Subtract (Vector4d a, Vector4d b)
        {
            Vector4d.Subtract (ref a, ref b, out a);
            return a;
        }
        
        public static void Subtract (ref Vector4d a, ref Vector4d b, out Vector4d result)
        {
            result = new Vector4d (a.X - b.X, a.Y - b.Y, a.Z - b.Z, a.W - b.W);
        }
        
        public static Vector4d Transform (Vector4d vec, Matrix4d mat)
        {
            Vector4d result;
            Vector4d.Transform (ref vec, ref mat, out result);
            return result;
        }
        
        public static void Transform (ref Vector4d vec, ref Matrix4d mat, out Vector4d result)
        {
            result = new Vector4d (vec.X * mat.Row0.X + vec.Y * mat.Row1.X + vec.Z * mat.Row2.X + vec.W * mat.Row3.X, vec.X * mat.Row0.Y + vec.Y * mat.Row1.Y + vec.Z * mat.Row2.Y + vec.W * mat.Row3.Y, vec.X * mat.Row0.Z + vec.Y * mat.Row1.Z + vec.Z * mat.Row2.Z + vec.W * mat.Row3.Z, vec.X * mat.Row0.W + vec.Y * mat.Row1.W + vec.Z * mat.Row2.W + vec.W * mat.Row3.W);
        }
        
        public static Vector4d Transform (Vector4d vec, Quaterniond quat)
        {
            Vector4d result;
            Vector4d.Transform (ref vec, ref quat, out result);
            return result;
        }
        
        public static void Transform (ref Vector4d vec, ref Quaterniond quat, out Vector4d result)
        {
            Quaterniond quaterniond = new Quaterniond (vec.X, vec.Y, vec.Z, vec.W);
            Quaterniond quaterniond2;
            Quaterniond.Invert (ref quat, out quaterniond2);
            Quaterniond quaterniond3;
            Quaterniond.Multiply (ref quat, ref quaterniond, out quaterniond3);
            Quaterniond.Multiply (ref quaterniond3, ref quaterniond2, out quaterniond);
            result = new Vector4d (quaterniond.X, quaterniond.Y, quaterniond.Z, quaterniond.W);
        }
        
        //
        // Methods
        //
        
        [Obsolete ("Use static Add() method instead.")]
        public void Add (Vector4d right)
        {
            this.X += right.X;
            this.Y += right.Y;
            this.Z += right.Z;
            this.W += right.W;
        }
        
        [CLSCompliant (false), Obsolete ("Use static Add() method instead.")]
        public void Add (ref Vector4d right)
        {
            this.X += right.X;
            this.Y += right.Y;
            this.Z += right.Z;
            this.W += right.W;
        }
        
        [Obsolete ("Use static Divide() method instead.")]
        public void Div (double f)
        {
            double num = 1.0 / f;
            this.X *= num;
            this.Y *= num;
            this.Z *= num;
            this.W *= num;
        }
        
        public bool Equals (Vector4d other)
        {
            return this.X == other.X && this.Y == other.Y && this.Z == other.Z && this.W == other.W;
        }
        
        public override bool Equals (object obj)
        {
            return obj is Vector4d && this.Equals ((Vector4d)obj);
        }
        
        public override int GetHashCode ()
        {
            return this.X.GetHashCode () ^ this.Y.GetHashCode () ^ this.Z.GetHashCode () ^ this.W.GetHashCode ();
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Mult (double f)
        {
            this.X *= f;
            this.Y *= f;
            this.Z *= f;
            this.W *= f;
        }
        
        public void Normalize ()
        {
            double num = 1.0 / this.Length;
            this.X *= num;
            this.Y *= num;
            this.Z *= num;
            this.W *= num;
        }
        
        public void NormalizeFast ()
        {
            double num = MathHelper.InverseSqrtFast (this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W);
            this.X *= num;
            this.Y *= num;
            this.Z *= num;
            this.W *= num;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Scale (Vector4d scale)
        {
            this.X *= scale.X;
            this.Y *= scale.Y;
            this.Z *= scale.Z;
            this.W *= scale.W;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Scale (double sx, double sy, double sz, double sw)
        {
            this.X *= sx;
            this.Y *= sy;
            this.Z *= sz;
            this.W *= sw;
        }
        
        [CLSCompliant (false), Obsolete ("Use static Multiply() method instead.")]
        public void Scale (ref Vector4d scale)
        {
            this.X *= scale.X;
            this.Y *= scale.Y;
            this.Z *= scale.Z;
            this.W *= scale.W;
        }
        
        [CLSCompliant (false), Obsolete ("Use static Subtract() method instead.")]
        public void Sub (ref Vector4d right)
        {
            this.X -= right.X;
            this.Y -= right.Y;
            this.Z -= right.Z;
            this.W -= right.W;
        }
        
        [Obsolete ("Use static Subtract() method instead.")]
        public void Sub (Vector4d right)
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
        
        public static Vector4d operator + (Vector4d left, Vector4d right)
        {
            left.X += right.X;
            left.Y += right.Y;
            left.Z += right.Z;
            left.W += right.W;
            return left;
        }
        
        public static Vector4d operator / (Vector4d vec, double scale)
        {
            double num = 1.0 / scale;
            vec.X *= num;
            vec.Y *= num;
            vec.Z *= num;
            vec.W *= num;
            return vec;
        }
        
        public static bool operator == (Vector4d left, Vector4d right)
        {
            return left.Equals (right);
        }
        
        public static explicit operator Vector4 (Vector4d v4d)
        {
            return new Vector4 ((float)v4d.X, (float)v4d.Y, (float)v4d.Z, (float)v4d.W);
        }
        
        public static explicit operator Vector4d (Vector4 v4)
        {
            return new Vector4d ((double)v4.X, (double)v4.Y, (double)v4.Z, (double)v4.W);
        }
        
        public unsafe static explicit operator IntPtr (Vector4d v)
        {
            return (IntPtr)((void*)(&v.X));
        }
        
        [CLSCompliant (false)]
        public unsafe static explicit operator double* (Vector4d v)
        {
            return &v.X;
        }
        
        public static bool operator != (Vector4d left, Vector4d right)
        {
            return !left.Equals (right);
        }
        
        public static Vector4d operator * (double scale, Vector4d vec)
        {
            vec.X *= scale;
            vec.Y *= scale;
            vec.Z *= scale;
            vec.W *= scale;
            return vec;
        }
        
        public static Vector4d operator * (Vector4d vec, double scale)
        {
            vec.X *= scale;
            vec.Y *= scale;
            vec.Z *= scale;
            vec.W *= scale;
            return vec;
        }
        
        public static Vector4d operator - (Vector4d left, Vector4d right)
        {
            left.X -= right.X;
            left.Y -= right.Y;
            left.Z -= right.Z;
            left.W -= right.W;
            return left;
        }
        
        public static Vector4d operator - (Vector4d vec)
        {
            vec.X = -vec.X;
            vec.Y = -vec.Y;
            vec.Z = -vec.Z;
            vec.W = -vec.W;
            return vec;
        }
    }
}
