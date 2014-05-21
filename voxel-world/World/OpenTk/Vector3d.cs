using System;
using System.Xml.Serialization;
namespace OpenTK
{
    public struct Vector3d : IEquatable<Vector3d>
    {
        //
        // Static Fields
        //
        
        public static readonly Vector3d One;
        
        public static readonly int SizeInBytes;
        
        public static readonly Vector3d UnitX;
        
        public static readonly Vector3d UnitY;
        
        public static readonly Vector3d UnitZ;
        
        public static readonly Vector3d Zero;
        
        //
        // Fields
        //
        
        public double X;
        
        public double Z;
        
        public double Y;
        
        //
        // Properties
        //
        
        public double Length
        {
            get
            {
                return Math.Sqrt (this.X * this.X + this.Y * this.Y + this.Z * this.Z);
            }
        }
        
        public double LengthFast
        {
            get
            {
                return 1.0 / MathHelper.InverseSqrtFast (this.X * this.X + this.Y * this.Y + this.Z * this.Z);
            }
        }
        
        public double LengthSquared
        {
            get
            {
                return this.X * this.X + this.Y * this.Y + this.Z * this.Z;
            }
        }
        
        [XmlIgnore]
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
        
        //
        // Constructors
        //
        
        public Vector3d (Vector4d v)
        {
            this.X = v.X;
            this.Y = v.Y;
            this.Z = v.Z;
        }
        
        public Vector3d (Vector3d v)
        {
            this.X = v.X;
            this.Y = v.Y;
            this.Z = v.Z;
        }
        
        public Vector3d (Vector2d v)
        {
            this.X = v.X;
            this.Y = v.Y;
            this.Z = 0.0;
        }
        
        public Vector3d (double x, double y, double z)
        {
            this.X = x;
            this.Y = y;
            this.Z = z;
        }
        
        //
        // Static Methods
        //
        
        public static void Add (ref Vector3d a, ref Vector3d b, out Vector3d result)
        {
            result = new Vector3d (a.X + b.X, a.Y + b.Y, a.Z + b.Z);
        }
        
        public static Vector3d Add (Vector3d a, Vector3d b)
        {
            Vector3d.Add (ref a, ref b, out a);
            return a;
        }
        
        public static void BaryCentric (ref Vector3d a, ref Vector3d b, ref Vector3d c, double u, double v, out Vector3d result)
        {
            result = a;
            Vector3d vector3d = b;
            Vector3d.Subtract (ref vector3d, ref a, out vector3d);
            Vector3d.Multiply (ref vector3d, u, out vector3d);
            Vector3d.Add (ref result, ref vector3d, out result);
            vector3d = c;
            Vector3d.Subtract (ref vector3d, ref a, out vector3d);
            Vector3d.Multiply (ref vector3d, v, out vector3d);
            Vector3d.Add (ref result, ref vector3d, out result);
        }
        
        public static Vector3d BaryCentric (Vector3d a, Vector3d b, Vector3d c, double u, double v)
        {
            return a + u * (b - a) + v * (c - a);
        }
        
        public static double CalculateAngle (Vector3d first, Vector3d second)
        {
            return Math.Acos (Vector3d.Dot (first, second) / (first.Length * second.Length));
        }
        
        public static void CalculateAngle (ref Vector3d first, ref Vector3d second, out double result)
        {
            double num;
            Vector3d.Dot (ref first, ref second, out num);
            result = Math.Acos (num / (first.Length * second.Length));
        }
        
        public static Vector3d Clamp (Vector3d vec, Vector3d min, Vector3d max)
        {
            vec.X = ((vec.X < min.X) ? min.X : ((vec.X > max.X) ? max.X : vec.X));
            vec.Y = ((vec.Y < min.Y) ? min.Y : ((vec.Y > max.Y) ? max.Y : vec.Y));
            vec.Z = ((vec.Z < min.Z) ? min.Z : ((vec.Z > max.Z) ? max.Z : vec.Z));
            return vec;
        }
        
        public static void Clamp (ref Vector3d vec, ref Vector3d min, ref Vector3d max, out Vector3d result)
        {
            result.X = ((vec.X < min.X) ? min.X : ((vec.X > max.X) ? max.X : vec.X));
            result.Y = ((vec.Y < min.Y) ? min.Y : ((vec.Y > max.Y) ? max.Y : vec.Y));
            result.Z = ((vec.Z < min.Z) ? min.Z : ((vec.Z > max.Z) ? max.Z : vec.Z));
        }
        
        public static void ComponentMax (ref Vector3d a, ref Vector3d b, out Vector3d result)
        {
            result.X = ((a.X > b.X) ? a.X : b.X);
            result.Y = ((a.Y > b.Y) ? a.Y : b.Y);
            result.Z = ((a.Z > b.Z) ? a.Z : b.Z);
        }
        
        public static Vector3d ComponentMax (Vector3d a, Vector3d b)
        {
            a.X = ((a.X > b.X) ? a.X : b.X);
            a.Y = ((a.Y > b.Y) ? a.Y : b.Y);
            a.Z = ((a.Z > b.Z) ? a.Z : b.Z);
            return a;
        }
        
        public static void ComponentMin (ref Vector3d a, ref Vector3d b, out Vector3d result)
        {
            result.X = ((a.X < b.X) ? a.X : b.X);
            result.Y = ((a.Y < b.Y) ? a.Y : b.Y);
            result.Z = ((a.Z < b.Z) ? a.Z : b.Z);
        }
        
        public static Vector3d ComponentMin (Vector3d a, Vector3d b)
        {
            a.X = ((a.X < b.X) ? a.X : b.X);
            a.Y = ((a.Y < b.Y) ? a.Y : b.Y);
            a.Z = ((a.Z < b.Z) ? a.Z : b.Z);
            return a;
        }
        
        public static void Cross (ref Vector3d left, ref Vector3d right, out Vector3d result)
        {
            result = new Vector3d (left.Y * right.Z - left.Z * right.Y, left.Z * right.X - left.X * right.Z, left.X * right.Y - left.Y * right.X);
        }
        
        public static Vector3d Cross (Vector3d left, Vector3d right)
        {
            Vector3d result;
            Vector3d.Cross (ref left, ref right, out result);
            return result;
        }
        
        [Obsolete ("Use static Divide() method instead.")]
        public static void Div (ref Vector3d a, double f, out Vector3d result)
        {
            double num = 1.0 / f;
            result.X = a.X * num;
            result.Y = a.Y * num;
            result.Z = a.Z * num;
        }
        
        [Obsolete ("Use static Divide() method instead.")]
        public static Vector3d Div (Vector3d a, double f)
        {
            double num = 1.0 / f;
            a.X *= num;
            a.Y *= num;
            a.Z *= num;
            return a;
        }
        
        public static Vector3d Divide (Vector3d vector, double scale)
        {
            Vector3d.Divide (ref vector, scale, out vector);
            return vector;
        }
        
        public static void Divide (ref Vector3d vector, ref Vector3d scale, out Vector3d result)
        {
            result = new Vector3d (vector.X / scale.X, vector.Y / scale.Y, vector.Z / scale.Z);
        }
        
        public static void Divide (ref Vector3d vector, double scale, out Vector3d result)
        {
            Vector3d.Multiply (ref vector, 1.0 / scale, out result);
        }
        
        public static Vector3d Divide (Vector3d vector, Vector3d scale)
        {
            Vector3d.Divide (ref vector, ref scale, out vector);
            return vector;
        }
        
        public static void Dot (ref Vector3d left, ref Vector3d right, out double result)
        {
            result = left.X * right.X + left.Y * right.Y + left.Z * right.Z;
        }
        
        public static double Dot (Vector3d left, Vector3d right)
        {
            return left.X * right.X + left.Y * right.Y + left.Z * right.Z;
        }
        
        public static void Lerp (ref Vector3d a, ref Vector3d b, double blend, out Vector3d result)
        {
            result.X = blend * (b.X - a.X) + a.X;
            result.Y = blend * (b.Y - a.Y) + a.Y;
            result.Z = blend * (b.Z - a.Z) + a.Z;
        }
        
        public static Vector3d Lerp (Vector3d a, Vector3d b, double blend)
        {
            a.X = blend * (b.X - a.X) + a.X;
            a.Y = blend * (b.Y - a.Y) + a.Y;
            a.Z = blend * (b.Z - a.Z) + a.Z;
            return a;
        }
        
        public static Vector3d Max (Vector3d left, Vector3d right)
        {
            if (left.LengthSquared < right.LengthSquared)
            {
                return right;
            }
            return left;
        }
        
        public static Vector3d Min (Vector3d left, Vector3d right)
        {
            if (left.LengthSquared >= right.LengthSquared)
            {
                return right;
            }
            return left;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public static void Mult (ref Vector3d a, double f, out Vector3d result)
        {
            result.X = a.X * f;
            result.Y = a.Y * f;
            result.Z = a.Z * f;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public static Vector3d Mult (Vector3d a, double f)
        {
            a.X *= f;
            a.Y *= f;
            a.Z *= f;
            return a;
        }
        
        public static void Multiply (ref Vector3d vector, ref Vector3d scale, out Vector3d result)
        {
            result = new Vector3d (vector.X * scale.X, vector.Y * scale.Y, vector.Z * scale.Z);
        }
        
        public static Vector3d Multiply (Vector3d vector, Vector3d scale)
        {
            Vector3d.Multiply (ref vector, ref scale, out vector);
            return vector;
        }
        
        public static void Multiply (ref Vector3d vector, double scale, out Vector3d result)
        {
            result = new Vector3d (vector.X * scale, vector.Y * scale, vector.Z * scale);
        }
        
        public static Vector3d Multiply (Vector3d vector, double scale)
        {
            Vector3d.Multiply (ref vector, scale, out vector);
            return vector;
        }
        
        public static void Normalize (ref Vector3d vec, out Vector3d result)
        {
            double num = 1.0 / vec.Length;
            result.X = vec.X * num;
            result.Y = vec.Y * num;
            result.Z = vec.Z * num;
        }
        
        public static Vector3d Normalize (Vector3d vec)
        {
            double num = 1.0 / vec.Length;
            vec.X *= num;
            vec.Y *= num;
            vec.Z *= num;
            return vec;
        }
        
        public static Vector3d NormalizeFast (Vector3d vec)
        {
            double num = MathHelper.InverseSqrtFast (vec.X * vec.X + vec.Y * vec.Y + vec.Z * vec.Z);
            vec.X *= num;
            vec.Y *= num;
            vec.Z *= num;
            return vec;
        }
        
        public static void NormalizeFast (ref Vector3d vec, out Vector3d result)
        {
            double num = MathHelper.InverseSqrtFast (vec.X * vec.X + vec.Y * vec.Y + vec.Z * vec.Z);
            result.X = vec.X * num;
            result.Y = vec.Y * num;
            result.Z = vec.Z * num;
        }
        
        [Obsolete ("Use static Subtract() method instead.")]
        public static void Sub (ref Vector3d a, ref Vector3d b, out Vector3d result)
        {
            result.X = a.X - b.X;
            result.Y = a.Y - b.Y;
            result.Z = a.Z - b.Z;
        }
        
        [Obsolete ("Use static Subtract() method instead.")]
        public static Vector3d Sub (Vector3d a, Vector3d b)
        {
            a.X -= b.X;
            a.Y -= b.Y;
            a.Z -= b.Z;
            return a;
        }
        
        public static Vector3d Subtract (Vector3d a, Vector3d b)
        {
            Vector3d.Subtract (ref a, ref b, out a);
            return a;
        }
        
        public static void Subtract (ref Vector3d a, ref Vector3d b, out Vector3d result)
        {
            result = new Vector3d (a.X - b.X, a.Y - b.Y, a.Z - b.Z);
        }
        
        public static Vector3d Transform (Vector3d vec, Matrix4d mat)
        {
            Vector3d result;
            Vector3d.Transform (ref vec, ref mat, out result);
            return result;
        }
        
        public static Vector3d Transform (Vector3d vec, Quaterniond quat)
        {
            Vector3d result;
            Vector3d.Transform (ref vec, ref quat, out result);
            return result;
        }
        
        public static void Transform (ref Vector3d vec, ref Quaterniond quat, out Vector3d result)
        {
            Vector3d xyz = quat.Xyz;
            Vector3d vector3d;
            Vector3d.Cross (ref xyz, ref vec, out vector3d);
            Vector3d vector3d2;
            Vector3d.Multiply (ref vec, quat.W, out vector3d2);
            Vector3d.Add (ref vector3d, ref vector3d2, out vector3d);
            Vector3d.Cross (ref xyz, ref vector3d, out vector3d);
            Vector3d.Multiply (ref vector3d, 2.0, out vector3d);
            Vector3d.Add (ref vec, ref vector3d, out result);
        }
        
        public static void Transform (ref Vector3d vec, ref Matrix4d mat, out Vector3d result)
        {
            Vector4d vector4d = new Vector4d (vec.X, vec.Y, vec.Z, 1.0);
            Vector4d.Transform (ref vector4d, ref mat, out vector4d);
            result = vector4d.Xyz;
        }
        
        public static void TransformNormal (ref Vector3d norm, ref Matrix4d mat, out Vector3d result)
        {
            Matrix4d matrix4d = Matrix4d.Invert (mat);
            Vector3d.TransformNormalInverse (ref norm, ref matrix4d, out result);
        }
        
        public static Vector3d TransformNormal (Vector3d norm, Matrix4d mat)
        {
            mat.Invert ();
            return Vector3d.TransformNormalInverse (norm, mat);
        }
        
        public static void TransformNormalInverse (ref Vector3d norm, ref Matrix4d invMat, out Vector3d result)
        {
            result.X = norm.X * invMat.Row0.X + norm.Y * invMat.Row0.Y + norm.Z * invMat.Row0.Z;
            result.Y = norm.X * invMat.Row1.X + norm.Y * invMat.Row1.Y + norm.Z * invMat.Row1.Z;
            result.Z = norm.X * invMat.Row2.X + norm.Y * invMat.Row2.Y + norm.Z * invMat.Row2.Z;
        }
        
        public static Vector3d TransformNormalInverse (Vector3d norm, Matrix4d invMat)
        {
            return new Vector3d (Vector3d.Dot (norm, new Vector3d (invMat.Row0)), Vector3d.Dot (norm, new Vector3d (invMat.Row1)), Vector3d.Dot (norm, new Vector3d (invMat.Row2)));
        }
        
        public static Vector3d TransformPerspective (Vector3d vec, Matrix4d mat)
        {
            Vector3d result;
            Vector3d.TransformPerspective (ref vec, ref mat, out result);
            return result;
        }
        
        public static void TransformPerspective (ref Vector3d vec, ref Matrix4d mat, out Vector3d result)
        {
            Vector4d vector4d = new Vector4d (vec);
            Vector4d.Transform (ref vector4d, ref mat, out vector4d);
            result.X = vector4d.X / vector4d.W;
            result.Y = vector4d.Y / vector4d.W;
            result.Z = vector4d.Z / vector4d.W;
        }
        
        public static Vector3d TransformPosition (Vector3d pos, Matrix4d mat)
        {
            return new Vector3d (Vector3d.Dot (pos, new Vector3d (mat.Column0)) + mat.Row3.X, Vector3d.Dot (pos, new Vector3d (mat.Column1)) + mat.Row3.Y, Vector3d.Dot (pos, new Vector3d (mat.Column2)) + mat.Row3.Z);
        }
        
        public static void TransformPosition (ref Vector3d pos, ref Matrix4d mat, out Vector3d result)
        {
            result.X = pos.X * mat.Row0.X + pos.Y * mat.Row1.X + pos.Z * mat.Row2.X + mat.Row3.X;
            result.Y = pos.X * mat.Row0.Y + pos.Y * mat.Row1.Y + pos.Z * mat.Row2.Y + mat.Row3.Y;
            result.Z = pos.X * mat.Row0.Z + pos.Y * mat.Row1.Z + pos.Z * mat.Row2.Z + mat.Row3.Z;
        }
        
        public static void TransformVector (ref Vector3d vec, ref Matrix4d mat, out Vector3d result)
        {
            result.X = vec.X * mat.Row0.X + vec.Y * mat.Row1.X + vec.Z * mat.Row2.X;
            result.Y = vec.X * mat.Row0.Y + vec.Y * mat.Row1.Y + vec.Z * mat.Row2.Y;
            result.Z = vec.X * mat.Row0.Z + vec.Y * mat.Row1.Z + vec.Z * mat.Row2.Z;
        }
        
        public static Vector3d TransformVector (Vector3d vec, Matrix4d mat)
        {
            return new Vector3d (Vector3d.Dot (vec, new Vector3d (mat.Column0)), Vector3d.Dot (vec, new Vector3d (mat.Column1)), Vector3d.Dot (vec, new Vector3d (mat.Column2)));
        }
        
        //
        // Methods
        //
        
        [Obsolete ("Use static Add() method instead.")]
        public void Add (Vector3d right)
        {
            this.X += right.X;
            this.Y += right.Y;
            this.Z += right.Z;
        }
        
        [CLSCompliant (false), Obsolete ("Use static Add() method instead.")]
        public void Add (ref Vector3d right)
        {
            this.X += right.X;
            this.Y += right.Y;
            this.Z += right.Z;
        }
        
        [Obsolete ("Use static Divide() method instead.")]
        public void Div (double f)
        {
            double num = 1.0 / f;
            this.X *= num;
            this.Y *= num;
            this.Z *= num;
        }
        
        public override bool Equals (object obj)
        {
            return obj is Vector3 && this.Equals ((Vector3)obj);
        }
        
        public bool Equals (Vector3d other)
        {
            return this.X == other.X && this.Y == other.Y && this.Z == other.Z;
        }
        
        public override int GetHashCode ()
        {
            return this.X.GetHashCode () ^ this.Y.GetHashCode () ^ this.Z.GetHashCode ();
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Mult (double f)
        {
            this.X *= f;
            this.Y *= f;
            this.Z *= f;
        }
        
        public void Normalize ()
        {
            double num = 1.0 / this.Length;
            this.X *= num;
            this.Y *= num;
            this.Z *= num;
        }
        
        public void NormalizeFast ()
        {
            double num = MathHelper.InverseSqrtFast (this.X * this.X + this.Y * this.Y + this.Z * this.Z);
            this.X *= num;
            this.Y *= num;
            this.Z *= num;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Scale (double sx, double sy, double sz)
        {
            this.X *= sx;
            this.Y *= sy;
            this.Z *= sz;
        }
        
        [CLSCompliant (false), Obsolete ("Use static Multiply() method instead.")]
        public void Scale (ref Vector3d scale)
        {
            this.X *= scale.X;
            this.Y *= scale.Y;
            this.Z *= scale.Z;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Scale (Vector3d scale)
        {
            this.X *= scale.X;
            this.Y *= scale.Y;
            this.Z *= scale.Z;
        }
        
        [Obsolete ("Use static Subtract() method instead.")]
        public void Sub (Vector3d right)
        {
            this.X -= right.X;
            this.Y -= right.Y;
            this.Z -= right.Z;
        }
        
        [CLSCompliant (false), Obsolete ("Use static Subtract() method instead.")]
        public void Sub (ref Vector3d right)
        {
            this.X -= right.X;
            this.Y -= right.Y;
            this.Z -= right.Z;
        }
        
        public override string ToString ()
        {
            return string.Format ("({0}, {1}, {2})", this.X, this.Y, this.Z);
        }
        
        //
        // Operators
        //
        
        public static Vector3d operator + (Vector3d left, Vector3d right)
        {
            left.X += right.X;
            left.Y += right.Y;
            left.Z += right.Z;
            return left;
        }
        
        public static Vector3d operator / (Vector3d vec, double scale)
        {
            double num = 1.0 / scale;
            vec.X *= num;
            vec.Y *= num;
            vec.Z *= num;
            return vec;
        }
        
        public static bool operator == (Vector3d left, Vector3d right)
        {
            return left.Equals (right);
        }
        
        public static explicit operator Vector3d (Vector3 v3)
        {
            return new Vector3d ((double)v3.X, (double)v3.Y, (double)v3.Z);
        }
        
        public static explicit operator Vector3 (Vector3d v3d)
        {
            return new Vector3 ((float)v3d.X, (float)v3d.Y, (float)v3d.Z);
        }
        
        public static bool operator != (Vector3d left, Vector3d right)
        {
            return !left.Equals (right);
        }
        
        public static Vector3d operator * (double scale, Vector3d vec)
        {
            vec.X *= scale;
            vec.Y *= scale;
            vec.Z *= scale;
            return vec;
        }
        
        public static Vector3d operator * (Vector3d vec, double scale)
        {
            vec.X *= scale;
            vec.Y *= scale;
            vec.Z *= scale;
            return vec;
        }
        
        public static Vector3d operator - (Vector3d left, Vector3d right)
        {
            left.X -= right.X;
            left.Y -= right.Y;
            left.Z -= right.Z;
            return left;
        }
        
        public static Vector3d operator - (Vector3d vec)
        {
            vec.X = -vec.X;
            vec.Y = -vec.Y;
            vec.Z = -vec.Z;
            return vec;
        }
    }
}
