using System;
using System.Xml.Serialization;
namespace OpenTK
{
    public struct Vector3 : IEquatable<Vector3>
    {
        //
        // Static Fields
        //
        
        public static readonly Vector3 One;
        
        public static readonly int SizeInBytes;
        
        public static readonly Vector3 UnitX;
        
        public static readonly Vector3 UnitY;
        
        public static readonly Vector3 UnitZ;
        
        public static readonly Vector3 Zero;
        
        //
        // Fields
        //
        
        public float X;
        
        public float Z;
        
        public float Y;
        
        //
        // Properties
        //
        
        public float Length
        {
            get
            {
                return (float)Math.Sqrt ((double)(this.X * this.X + this.Y * this.Y + this.Z * this.Z));
            }
        }
        
        public float LengthFast
        {
            get
            {
                return 1f / MathHelper.InverseSqrtFast (this.X * this.X + this.Y * this.Y + this.Z * this.Z);
            }
        }
        
        public float LengthSquared
        {
            get
            {
                return this.X * this.X + this.Y * this.Y + this.Z * this.Z;
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
        
        //
        // Constructors
        //
        
        public Vector3 (Vector4 v)
        {
            this.X = v.X;
            this.Y = v.Y;
            this.Z = v.Z;
        }
        
        public Vector3 (Vector3 v)
        {
            this.X = v.X;
            this.Y = v.Y;
            this.Z = v.Z;
        }
        
        public Vector3 (Vector2 v)
        {
            this.X = v.X;
            this.Y = v.Y;
            this.Z = 0f;
        }
        
        public Vector3 (float x, float y, float z)
        {
            this.X = x;
            this.Y = y;
            this.Z = z;
        }
        
        //
        // Static Methods
        //
        
        public static void Add (ref Vector3 a, ref Vector3 b, out Vector3 result)
        {
            result = new Vector3 (a.X + b.X, a.Y + b.Y, a.Z + b.Z);
        }
        
        public static Vector3 Add (Vector3 a, Vector3 b)
        {
            Vector3.Add (ref a, ref b, out a);
            return a;
        }
        
        public static void BaryCentric (ref Vector3 a, ref Vector3 b, ref Vector3 c, float u, float v, out Vector3 result)
        {
            result = a;
            Vector3 vector = b;
            Vector3.Subtract (ref vector, ref a, out vector);
            Vector3.Multiply (ref vector, u, out vector);
            Vector3.Add (ref result, ref vector, out result);
            vector = c;
            Vector3.Subtract (ref vector, ref a, out vector);
            Vector3.Multiply (ref vector, v, out vector);
            Vector3.Add (ref result, ref vector, out result);
        }
        
        public static Vector3 BaryCentric (Vector3 a, Vector3 b, Vector3 c, float u, float v)
        {
            return a + u * (b - a) + v * (c - a);
        }
        
        public static float CalculateAngle (Vector3 first, Vector3 second)
        {
            return (float)Math.Acos ((double)(Vector3.Dot (first, second) / (first.Length * second.Length)));
        }
        
        public static void CalculateAngle (ref Vector3 first, ref Vector3 second, out float result)
        {
            float num;
            Vector3.Dot (ref first, ref second, out num);
            result = (float)Math.Acos ((double)(num / (first.Length * second.Length)));
        }
        
        public static Vector3 Clamp (Vector3 vec, Vector3 min, Vector3 max)
        {
            vec.X = ((vec.X < min.X) ? min.X : ((vec.X > max.X) ? max.X : vec.X));
            vec.Y = ((vec.Y < min.Y) ? min.Y : ((vec.Y > max.Y) ? max.Y : vec.Y));
            vec.Z = ((vec.Z < min.Z) ? min.Z : ((vec.Z > max.Z) ? max.Z : vec.Z));
            return vec;
        }
        
        public static void Clamp (ref Vector3 vec, ref Vector3 min, ref Vector3 max, out Vector3 result)
        {
            result.X = ((vec.X < min.X) ? min.X : ((vec.X > max.X) ? max.X : vec.X));
            result.Y = ((vec.Y < min.Y) ? min.Y : ((vec.Y > max.Y) ? max.Y : vec.Y));
            result.Z = ((vec.Z < min.Z) ? min.Z : ((vec.Z > max.Z) ? max.Z : vec.Z));
        }
        
        public static void ComponentMax (ref Vector3 a, ref Vector3 b, out Vector3 result)
        {
            result.X = ((a.X > b.X) ? a.X : b.X);
            result.Y = ((a.Y > b.Y) ? a.Y : b.Y);
            result.Z = ((a.Z > b.Z) ? a.Z : b.Z);
        }
        
        public static Vector3 ComponentMax (Vector3 a, Vector3 b)
        {
            a.X = ((a.X > b.X) ? a.X : b.X);
            a.Y = ((a.Y > b.Y) ? a.Y : b.Y);
            a.Z = ((a.Z > b.Z) ? a.Z : b.Z);
            return a;
        }
        
        public static void ComponentMin (ref Vector3 a, ref Vector3 b, out Vector3 result)
        {
            result.X = ((a.X < b.X) ? a.X : b.X);
            result.Y = ((a.Y < b.Y) ? a.Y : b.Y);
            result.Z = ((a.Z < b.Z) ? a.Z : b.Z);
        }
        
        public static Vector3 ComponentMin (Vector3 a, Vector3 b)
        {
            a.X = ((a.X < b.X) ? a.X : b.X);
            a.Y = ((a.Y < b.Y) ? a.Y : b.Y);
            a.Z = ((a.Z < b.Z) ? a.Z : b.Z);
            return a;
        }
        
        public static void Cross (ref Vector3 left, ref Vector3 right, out Vector3 result)
        {
            result = new Vector3 (left.Y * right.Z - left.Z * right.Y, left.Z * right.X - left.X * right.Z, left.X * right.Y - left.Y * right.X);
        }
        
        public static Vector3 Cross (Vector3 left, Vector3 right)
        {
            Vector3 result;
            Vector3.Cross (ref left, ref right, out result);
            return result;
        }
        
        [Obsolete ("Use static Divide() method instead.")]
        public static void Div (ref Vector3 a, float f, out Vector3 result)
        {
            float num = 1f / f;
            result.X = a.X * num;
            result.Y = a.Y * num;
            result.Z = a.Z * num;
        }
        
        [Obsolete ("Use static Divide() method instead.")]
        public static Vector3 Div (Vector3 a, float f)
        {
            float num = 1f / f;
            a.X *= num;
            a.Y *= num;
            a.Z *= num;
            return a;
        }
        
        public static Vector3 Divide (Vector3 vector, float scale)
        {
            Vector3.Divide (ref vector, scale, out vector);
            return vector;
        }
        
        public static void Divide (ref Vector3 vector, ref Vector3 scale, out Vector3 result)
        {
            result = new Vector3 (vector.X / scale.X, vector.Y / scale.Y, vector.Z / scale.Z);
        }
        
        public static void Divide (ref Vector3 vector, float scale, out Vector3 result)
        {
            Vector3.Multiply (ref vector, 1f / scale, out result);
        }
        
        public static Vector3 Divide (Vector3 vector, Vector3 scale)
        {
            Vector3.Divide (ref vector, ref scale, out vector);
            return vector;
        }
        
        public static void Dot (ref Vector3 left, ref Vector3 right, out float result)
        {
            result = left.X * right.X + left.Y * right.Y + left.Z * right.Z;
        }
        
        public static float Dot (Vector3 left, Vector3 right)
        {
            return left.X * right.X + left.Y * right.Y + left.Z * right.Z;
        }
        
        public static Vector3 Lerp (Vector3 a, Vector3 b, float blend)
        {
            a.X = blend * (b.X - a.X) + a.X;
            a.Y = blend * (b.Y - a.Y) + a.Y;
            a.Z = blend * (b.Z - a.Z) + a.Z;
            return a;
        }
        
        public static void Lerp (ref Vector3 a, ref Vector3 b, float blend, out Vector3 result)
        {
            result.X = blend * (b.X - a.X) + a.X;
            result.Y = blend * (b.Y - a.Y) + a.Y;
            result.Z = blend * (b.Z - a.Z) + a.Z;
        }
        
        public static Vector3 Max (Vector3 left, Vector3 right)
        {
            if (left.LengthSquared < right.LengthSquared)
            {
                return right;
            }
            return left;
        }
        
        public static Vector3 Min (Vector3 left, Vector3 right)
        {
            if (left.LengthSquared >= right.LengthSquared)
            {
                return right;
            }
            return left;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public static void Mult (ref Vector3 a, float f, out Vector3 result)
        {
            result.X = a.X * f;
            result.Y = a.Y * f;
            result.Z = a.Z * f;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public static Vector3 Mult (Vector3 a, float f)
        {
            a.X *= f;
            a.Y *= f;
            a.Z *= f;
            return a;
        }
        
        public static void Multiply (ref Vector3 vector, ref Vector3 scale, out Vector3 result)
        {
            result = new Vector3 (vector.X * scale.X, vector.Y * scale.Y, vector.Z * scale.Z);
        }
        
        public static Vector3 Multiply (Vector3 vector, Vector3 scale)
        {
            Vector3.Multiply (ref vector, ref scale, out vector);
            return vector;
        }
        
        public static void Multiply (ref Vector3 vector, float scale, out Vector3 result)
        {
            result = new Vector3 (vector.X * scale, vector.Y * scale, vector.Z * scale);
        }
        
        public static Vector3 Multiply (Vector3 vector, float scale)
        {
            Vector3.Multiply (ref vector, scale, out vector);
            return vector;
        }
        
        public static void Normalize (ref Vector3 vec, out Vector3 result)
        {
            float num = 1f / vec.Length;
            result.X = vec.X * num;
            result.Y = vec.Y * num;
            result.Z = vec.Z * num;
        }
        
        public static Vector3 Normalize (Vector3 vec)
        {
            float num = 1f / vec.Length;
            vec.X *= num;
            vec.Y *= num;
            vec.Z *= num;
            return vec;
        }
        
        public static Vector3 NormalizeFast (Vector3 vec)
        {
            float num = MathHelper.InverseSqrtFast (vec.X * vec.X + vec.Y * vec.Y + vec.Z * vec.Z);
            vec.X *= num;
            vec.Y *= num;
            vec.Z *= num;
            return vec;
        }
        
        public static void NormalizeFast (ref Vector3 vec, out Vector3 result)
        {
            float num = MathHelper.InverseSqrtFast (vec.X * vec.X + vec.Y * vec.Y + vec.Z * vec.Z);
            result.X = vec.X * num;
            result.Y = vec.Y * num;
            result.Z = vec.Z * num;
        }
        
        [Obsolete ("Use static Subtract() method instead.")]
        public static void Sub (ref Vector3 a, ref Vector3 b, out Vector3 result)
        {
            result.X = a.X - b.X;
            result.Y = a.Y - b.Y;
            result.Z = a.Z - b.Z;
        }
        
        [Obsolete ("Use static Subtract() method instead.")]
        public static Vector3 Sub (Vector3 a, Vector3 b)
        {
            a.X -= b.X;
            a.Y -= b.Y;
            a.Z -= b.Z;
            return a;
        }
        
        public static Vector3 Subtract (Vector3 a, Vector3 b)
        {
            Vector3.Subtract (ref a, ref b, out a);
            return a;
        }
        
        public static void Subtract (ref Vector3 a, ref Vector3 b, out Vector3 result)
        {
            result = new Vector3 (a.X - b.X, a.Y - b.Y, a.Z - b.Z);
        }
        
        public static Vector3 Transform (Vector3 vec, Quaternion quat)
        {
            Vector3 result;
            Vector3.Transform (ref vec, ref quat, out result);
            return result;
        }
        
        public static void Transform (ref Vector3 vec, ref Matrix4 mat, out Vector3 result)
        {
            Vector4 vector = new Vector4 (vec.X, vec.Y, vec.Z, 1f);
            Vector4.Transform (ref vector, ref mat, out vector);
            result = vector.Xyz;
        }
        
        public static Vector3 Transform (Vector3 vec, Matrix4 mat)
        {
            Vector3 result;
            Vector3.Transform (ref vec, ref mat, out result);
            return result;
        }
        
        public static void Transform (ref Vector3 vec, ref Quaternion quat, out Vector3 result)
        {
            Vector3 xyz = quat.Xyz;
            Vector3 vector;
            Vector3.Cross (ref xyz, ref vec, out vector);
            Vector3 vector2;
            Vector3.Multiply (ref vec, quat.W, out vector2);
            Vector3.Add (ref vector, ref vector2, out vector);
            Vector3.Cross (ref xyz, ref vector, out vector);
            Vector3.Multiply (ref vector, 2f, out vector);
            Vector3.Add (ref vec, ref vector, out result);
        }
        
        public static void TransformNormal (ref Vector3 norm, ref Matrix4 mat, out Vector3 result)
        {
            Matrix4 matrix = Matrix4.Invert (mat);
            Vector3.TransformNormalInverse (ref norm, ref matrix, out result);
        }
        
        public static Vector3 TransformNormal (Vector3 norm, Matrix4 mat)
        {
            mat.Invert ();
            return Vector3.TransformNormalInverse (norm, mat);
        }
        
        public static void TransformNormalInverse (ref Vector3 norm, ref Matrix4 invMat, out Vector3 result)
        {
            result.X = norm.X * invMat.Row0.X + norm.Y * invMat.Row0.Y + norm.Z * invMat.Row0.Z;
            result.Y = norm.X * invMat.Row1.X + norm.Y * invMat.Row1.Y + norm.Z * invMat.Row1.Z;
            result.Z = norm.X * invMat.Row2.X + norm.Y * invMat.Row2.Y + norm.Z * invMat.Row2.Z;
        }
        
        public static Vector3 TransformNormalInverse (Vector3 norm, Matrix4 invMat)
        {
            Vector3 result;
            result.X = Vector3.Dot (norm, new Vector3 (invMat.Row0));
            result.Y = Vector3.Dot (norm, new Vector3 (invMat.Row1));
            result.Z = Vector3.Dot (norm, new Vector3 (invMat.Row2));
            return result;
        }
        
        public static void TransformPerspective (ref Vector3 vec, ref Matrix4 mat, out Vector3 result)
        {
            Vector4 vector = new Vector4 (vec);
            Vector4.Transform (ref vector, ref mat, out vector);
            result.X = vector.X / vector.W;
            result.Y = vector.Y / vector.W;
            result.Z = vector.Z / vector.W;
        }
        
        public static Vector3 TransformPerspective (Vector3 vec, Matrix4 mat)
        {
            Vector3 result;
            Vector3.TransformPerspective (ref vec, ref mat, out result);
            return result;
        }
        
        public static Vector3 TransformPosition (Vector3 pos, Matrix4 mat)
        {
            Vector3 result;
            result.X = Vector3.Dot (pos, new Vector3 (mat.Column0)) + mat.Row3.X;
            result.Y = Vector3.Dot (pos, new Vector3 (mat.Column1)) + mat.Row3.Y;
            result.Z = Vector3.Dot (pos, new Vector3 (mat.Column2)) + mat.Row3.Z;
            return result;
        }
        
        public static void TransformPosition (ref Vector3 pos, ref Matrix4 mat, out Vector3 result)
        {
            result.X = pos.X * mat.Row0.X + pos.Y * mat.Row1.X + pos.Z * mat.Row2.X + mat.Row3.X;
            result.Y = pos.X * mat.Row0.Y + pos.Y * mat.Row1.Y + pos.Z * mat.Row2.Y + mat.Row3.Y;
            result.Z = pos.X * mat.Row0.Z + pos.Y * mat.Row1.Z + pos.Z * mat.Row2.Z + mat.Row3.Z;
        }
        
        public static void TransformVector (ref Vector3 vec, ref Matrix4 mat, out Vector3 result)
        {
            result.X = vec.X * mat.Row0.X + vec.Y * mat.Row1.X + vec.Z * mat.Row2.X;
            result.Y = vec.X * mat.Row0.Y + vec.Y * mat.Row1.Y + vec.Z * mat.Row2.Y;
            result.Z = vec.X * mat.Row0.Z + vec.Y * mat.Row1.Z + vec.Z * mat.Row2.Z;
        }
        
        public static Vector3 TransformVector (Vector3 vec, Matrix4 mat)
        {
            Vector3 result;
            result.X = Vector3.Dot (vec, new Vector3 (mat.Column0));
            result.Y = Vector3.Dot (vec, new Vector3 (mat.Column1));
            result.Z = Vector3.Dot (vec, new Vector3 (mat.Column2));
            return result;
        }
        
        //
        // Methods
        //
        
        [Obsolete ("Use static Add() method instead.")]
        public void Add (Vector3 right)
        {
            this.X += right.X;
            this.Y += right.Y;
            this.Z += right.Z;
        }
        
        [CLSCompliant (false), Obsolete ("Use static Add() method instead.")]
        public void Add (ref Vector3 right)
        {
            this.X += right.X;
            this.Y += right.Y;
            this.Z += right.Z;
        }
        
        [Obsolete ("Use static Divide() method instead.")]
        public void Div (float f)
        {
            float num = 1f / f;
            this.X *= num;
            this.Y *= num;
            this.Z *= num;
        }
        
        public bool Equals (Vector3 other)
        {
            return this.X == other.X && this.Y == other.Y && this.Z == other.Z;
        }
        
        public override bool Equals (object obj)
        {
            return obj is Vector3 && this.Equals ((Vector3)obj);
        }
        
        public override int GetHashCode ()
        {
            return this.X.GetHashCode () ^ this.Y.GetHashCode () ^ this.Z.GetHashCode ();
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Mult (float f)
        {
            this.X *= f;
            this.Y *= f;
            this.Z *= f;
        }
        
        public void Normalize ()
        {
            float num = 1f / this.Length;
            this.X *= num;
            this.Y *= num;
            this.Z *= num;
        }
        
        public void NormalizeFast ()
        {
            float num = MathHelper.InverseSqrtFast (this.X * this.X + this.Y * this.Y + this.Z * this.Z);
            this.X *= num;
            this.Y *= num;
            this.Z *= num;
        }
        
        [CLSCompliant (false), Obsolete ("Use static Multiply() method instead.")]
        public void Scale (ref Vector3 scale)
        {
            this.X *= scale.X;
            this.Y *= scale.Y;
            this.Z *= scale.Z;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Scale (Vector3 scale)
        {
            this.X *= scale.X;
            this.Y *= scale.Y;
            this.Z *= scale.Z;
        }
        
        [Obsolete ("Use static Multiply() method instead.")]
        public void Scale (float sx, float sy, float sz)
        {
            this.X *= sx;
            this.Y *= sy;
            this.Z *= sz;
        }
        
        [Obsolete ("Use static Subtract() method instead.")]
        public void Sub (Vector3 right)
        {
            this.X -= right.X;
            this.Y -= right.Y;
            this.Z -= right.Z;
        }
        
        [CLSCompliant (false), Obsolete ("Use static Subtract() method instead.")]
        public void Sub (ref Vector3 right)
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
        
        public static Vector3 operator + (Vector3 left, Vector3 right)
        {
            left.X += right.X;
            left.Y += right.Y;
            left.Z += right.Z;
            return left;
        }
        
        public static Vector3 operator / (Vector3 vec, float scale)
        {
            float num = 1f / scale;
            vec.X *= num;
            vec.Y *= num;
            vec.Z *= num;
            return vec;
        }
        
        public static bool operator == (Vector3 left, Vector3 right)
        {
            return left.Equals (right);
        }
        
        public static bool operator != (Vector3 left, Vector3 right)
        {
            return !left.Equals (right);
        }
        
        public static Vector3 operator * (float scale, Vector3 vec)
        {
            vec.X *= scale;
            vec.Y *= scale;
            vec.Z *= scale;
            return vec;
        }
        
        public static Vector3 operator * (Vector3 vec, float scale)
        {
            vec.X *= scale;
            vec.Y *= scale;
            vec.Z *= scale;
            return vec;
        }
        
        public static Vector3 operator - (Vector3 left, Vector3 right)
        {
            left.X -= right.X;
            left.Y -= right.Y;
            left.Z -= right.Z;
            return left;
        }
        
        public static Vector3 operator - (Vector3 vec)
        {
            vec.X = -vec.X;
            vec.Y = -vec.Y;
            vec.Z = -vec.Z;
            return vec;
        }
    }
}
