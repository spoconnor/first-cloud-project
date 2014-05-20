using System;
using System.ComponentModel;
namespace OpenTK
{
    [Serializable]
    public struct Quaternion : IEquatable<Quaternion>
    {
        //
        // Static Fields
        //
        
        public static Quaternion Identity;
        
        //
        // Properties
        //
        
        public float Length
        {
            get
            {
                return (float)Math.Sqrt ((double)(this.W * this.W + this.Xyz.LengthSquared));
            }
        }
        
        public float LengthSquared
        {
            get
            {
                return this.W * this.W + this.Xyz.LengthSquared;
            }
        }
        
        public float W
        {
            get
            {
                return this.w;
            }
            set
            {
                this.w = value;
            }
        }
        
        [XmlIgnore]
        public float X
        {
            get
            {
                return this.xyz.X;
            }
            set
            {
                this.xyz.X = value;
            }
        }
        
        public Vector3 Xyz
        {
            get
            {
                return this.xyz;
            }
            set
            {
                this.xyz = value;
            }
        }
        
        [CLSCompliant (false), EditorBrowsable (EditorBrowsableState.Never), Obsolete ("Use Xyz property instead."), XmlIgnore]
        public Vector3 XYZ
        {
            get
            {
                return this.Xyz;
            }
            set
            {
                this.Xyz = value;
            }
        }
        
        [XmlIgnore]
        public float Y
        {
            get
            {
                return this.xyz.Y;
            }
            set
            {
                this.xyz.Y = value;
            }
        }
        
        [XmlIgnore]
        public float Z
        {
            get
            {
                return this.xyz.Z;
            }
            set
            {
                this.xyz.Z = value;
            }
        }
        
        //
        // Constructors
        //
        
        public Quaternion (float x, float y, float z, float w)
        {
            this = new Quaternion (new Vector3 (x, y, z), w);
        }
        
        public Quaternion (Vector3 v, float w)
        {
            this.xyz = v;
            this.w = w;
        }
        
        //
        // Static Methods
        //
        
        public static void Add (ref Quaternion left, ref Quaternion right, out Quaternion result)
        {
            result = new Quaternion (left.Xyz + right.Xyz, left.W + right.W);
        }
        
        public static Quaternion Add (Quaternion left, Quaternion right)
        {
            return new Quaternion (left.Xyz + right.Xyz, left.W + right.W);
        }
        
        public static Quaternion Conjugate (Quaternion q)
        {
            return new Quaternion (-q.Xyz, q.W);
        }
        
        public static void Conjugate (ref Quaternion q, out Quaternion result)
        {
            result = new Quaternion (-q.Xyz, q.W);
        }
        
        public static Quaternion FromAxisAngle (Vector3 axis, float angle)
        {
            if (axis.LengthSquared == 0f)
            {
                return Quaternion.Identity;
            }
            Quaternion identity = Quaternion.Identity;
            angle *= 0.5f;
            axis.Normalize ();
            identity.Xyz = axis * (float)Math.Sin ((double)angle);
            identity.W = (float)Math.Cos ((double)angle);
            return Quaternion.Normalize (identity);
        }
        
        public static Quaternion Invert (Quaternion q)
        {
            Quaternion result;
            Quaternion.Invert (ref q, out result);
            return result;
        }
        
        public static void Invert (ref Quaternion q, out Quaternion result)
        {
            float lengthSquared = q.LengthSquared;
            if ((double)lengthSquared != 0.0)
            {
                float num = 1f / lengthSquared;
                result = new Quaternion (q.Xyz * -num, q.W * num);
                return;
            }
            result = q;
        }
        
        [Obsolete ("Use Multiply instead.")]
        public static Quaternion Mult (Quaternion left, Quaternion right)
        {
            return new Quaternion (right.W * left.Xyz + left.W * right.Xyz + Vector3.Cross (left.Xyz, right.Xyz), left.W * right.W - Vector3.Dot (left.Xyz, right.Xyz));
        }
        
        [Obsolete ("Use Multiply instead.")]
        public static void Mult (ref Quaternion left, ref Quaternion right, out Quaternion result)
        {
            result = new Quaternion (right.W * left.Xyz + left.W * right.Xyz + Vector3.Cross (left.Xyz, right.Xyz), left.W * right.W - Vector3.Dot (left.Xyz, right.Xyz));
        }
        
        public static void Multiply (ref Quaternion quaternion, float scale, out Quaternion result)
        {
            result = new Quaternion (quaternion.X * scale, quaternion.Y * scale, quaternion.Z * scale, quaternion.W * scale);
        }
        
        public static Quaternion Multiply (Quaternion quaternion, float scale)
        {
            return new Quaternion (quaternion.X * scale, quaternion.Y * scale, quaternion.Z * scale, quaternion.W * scale);
        }
        
        public static void Multiply (ref Quaternion left, ref Quaternion right, out Quaternion result)
        {
            result = new Quaternion (right.W * left.Xyz + left.W * right.Xyz + Vector3.Cross (left.Xyz, right.Xyz), left.W * right.W - Vector3.Dot (left.Xyz, right.Xyz));
        }
        
        public static Quaternion Multiply (Quaternion left, Quaternion right)
        {
            Quaternion result;
            Quaternion.Multiply (ref left, ref right, out result);
            return result;
        }
        
        public static void Normalize (ref Quaternion q, out Quaternion result)
        {
            float num = 1f / q.Length;
            result = new Quaternion (q.Xyz * num, q.W * num);
        }
        
        public static Quaternion Normalize (Quaternion q)
        {
            Quaternion result;
            Quaternion.Normalize (ref q, out result);
            return result;
        }
        
        public static Quaternion Slerp (Quaternion q1, Quaternion q2, float blend)
        {
            if (q1.LengthSquared == 0f)
            {
                if (q2.LengthSquared == 0f)
                {
                    return Quaternion.Identity;
                }
                return q2;
            }
            else
            {
                if (q2.LengthSquared == 0f)
                {
                    return q1;
                }
                float num = q1.W * q2.W + Vector3.Dot (q1.Xyz, q2.Xyz);
                if (num >= 1f || num <= -1f)
                {
                    return q1;
                }
                if (num < 0f)
                {
                    q2.Xyz = -q2.Xyz;
                    q2.W = -q2.W;
                    num = -num;
                }
                float num5;
                float num6;
                if (num < 0.99f)
                {
                    float num2 = (float)Math.Acos ((double)num);
                    float num3 = (float)Math.Sin ((double)num2);
                    float num4 = 1f / num3;
                    num5 = (float)Math.Sin ((double)(num2 * (1f - blend))) * num4;
                    num6 = (float)Math.Sin ((double)(num2 * blend)) * num4;
                }
                else
                {
                    num5 = 1f - blend;
                    num6 = blend;
                }
                Quaternion q3 = new Quaternion (num5 * q1.Xyz + num6 * q2.Xyz, num5 * q1.W + num6 * q2.W);
                if (q3.LengthSquared > 0f)
                {
                    return Quaternion.Normalize (q3);
                }
                return Quaternion.Identity;
            }
        }
        
        public static Quaternion Sub (Quaternion left, Quaternion right)
        {
            return new Quaternion (left.Xyz - right.Xyz, left.W - right.W);
        }
        
        public static void Sub (ref Quaternion left, ref Quaternion right, out Quaternion result)
        {
            result = new Quaternion (left.Xyz - right.Xyz, left.W - right.W);
        }
        
        //
        // Methods
        //
        
        public void Conjugate ()
        {
            this.Xyz = -this.Xyz;
        }
        
        public override bool Equals (object other)
        {
            return other is Quaternion && this == (Quaternion)other;
        }
        
        public bool Equals (Quaternion other)
        {
            return this.Xyz == other.Xyz && this.W == other.W;
        }
        
        public override int GetHashCode ()
        {
            return this.Xyz.GetHashCode () ^ this.W.GetHashCode ();
        }
        
        public void Normalize ()
        {
            float num = 1f / this.Length;
            this.Xyz *= num;
            this.W *= num;
        }
        
        public Vector4 ToAxisAngle ()
        {
            Quaternion quaternion = this;
            if (quaternion.W > 1f)
            {
                quaternion.Normalize ();
            }
            Vector4 result = default(Vector4);
            result.W = 2f * (float)Math.Acos ((double)quaternion.W);
            float num = (float)Math.Sqrt (1.0 - (double)(quaternion.W * quaternion.W));
            if (num > 0.0001f)
            {
                result.Xyz = quaternion.Xyz / num;
            }
            else
            {
                result.Xyz = Vector3.UnitX;
            }
            return result;
        }
        
        public void ToAxisAngle (out Vector3 axis, out float angle)
        {
            Vector4 vector = this.ToAxisAngle ();
            axis = vector.Xyz;
            angle = vector.W;
        }
        
        public override string ToString ()
        {
            return string.Format ("V: {0}, W: {1}", this.Xyz, this.W);
        }
        
        //
        // Operators
        //
        
        public static Quaternion operator + (Quaternion left, Quaternion right)
        {
            left.Xyz += right.Xyz;
            left.W += right.W;
            return left;
        }
        
        public static bool operator == (Quaternion left, Quaternion right)
        {
            return left.Equals (right);
        }
        
        public static bool operator != (Quaternion left, Quaternion right)
        {
            return !left.Equals (right);
        }
        
        public static Quaternion operator * (Quaternion left, Quaternion right)
        {
            Quaternion.Multiply (ref left, ref right, out left);
            return left;
        }
        
        public static Quaternion operator * (float scale, Quaternion quaternion)
        {
            return new Quaternion (quaternion.X * scale, quaternion.Y * scale, quaternion.Z * scale, quaternion.W * scale);
        }
        
        public static Quaternion operator * (Quaternion quaternion, float scale)
        {
            Quaternion.Multiply (ref quaternion, scale, out quaternion);
            return quaternion;
        }
        
        public static Quaternion operator - (Quaternion left, Quaternion right)
        {
            left.Xyz -= right.Xyz;
            left.W -= right.W;
            return left;
        }
    }
}
