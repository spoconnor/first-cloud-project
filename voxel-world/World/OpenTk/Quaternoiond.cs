using System;
using System.ComponentModel;
namespace OpenTK
{
    public struct Quaterniond : IEquatable<Quaterniond>
    {
        //
        // Static Fields
        //
        
        public static readonly Quaterniond Identity;
        
        //
        // Properties
        //
        
        public double Length
        {
            get
            {
                return Math.Sqrt (this.W * this.W + this.Xyz.LengthSquared);
            }
        }
        
        public double LengthSquared
        {
            get
            {
                return this.W * this.W + this.Xyz.LengthSquared;
            }
        }
        
        public double W
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
        public double X
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
        
        public Vector3d Xyz
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
        public Vector3d XYZ
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
        public double Y
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
        public double Z
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
        
        public Quaterniond (double x, double y, double z, double w)
        {
            this = new Quaterniond (new Vector3d (x, y, z), w);
        }
        
        public Quaterniond (Vector3d v, double w)
        {
            this.xyz = v;
            this.w = w;
        }
        
        //
        // Static Methods
        //
        
        public static void Add (ref Quaterniond left, ref Quaterniond right, out Quaterniond result)
        {
            result = new Quaterniond (left.Xyz + right.Xyz, left.W + right.W);
        }
        
        public static Quaterniond Add (Quaterniond left, Quaterniond right)
        {
            return new Quaterniond (left.Xyz + right.Xyz, left.W + right.W);
        }
        
        public static Quaterniond Conjugate (Quaterniond q)
        {
            return new Quaterniond (-q.Xyz, q.W);
        }
        
        public static void Conjugate (ref Quaterniond q, out Quaterniond result)
        {
            result = new Quaterniond (-q.Xyz, q.W);
        }
        
        public static Quaterniond FromAxisAngle (Vector3d axis, double angle)
        {
            if (axis.LengthSquared == 0.0)
            {
                return Quaterniond.Identity;
            }
            Quaterniond identity = Quaterniond.Identity;
            angle *= 0.5;
            axis.Normalize ();
            identity.Xyz = axis * Math.Sin (angle);
            identity.W = Math.Cos (angle);
            return Quaterniond.Normalize (identity);
        }
        
        public static Quaterniond Invert (Quaterniond q)
        {
            Quaterniond result;
            Quaterniond.Invert (ref q, out result);
            return result;
        }
        
        public static void Invert (ref Quaterniond q, out Quaterniond result)
        {
            double lengthSquared = q.LengthSquared;
            if (lengthSquared != 0.0)
            {
                double num = 1.0 / lengthSquared;
                result = new Quaterniond (q.Xyz * -num, q.W * num);
                return;
            }
            result = q;
        }
        
        [Obsolete ("Use Multiply instead.")]
        public static Quaterniond Mult (Quaterniond left, Quaterniond right)
        {
            return new Quaterniond (right.W * left.Xyz + left.W * right.Xyz + Vector3d.Cross (left.Xyz, right.Xyz), left.W * right.W - Vector3d.Dot (left.Xyz, right.Xyz));
        }
        
        [Obsolete ("Use Multiply instead.")]
        public static void Mult (ref Quaterniond left, ref Quaterniond right, out Quaterniond result)
        {
            result = new Quaterniond (right.W * left.Xyz + left.W * right.Xyz + Vector3d.Cross (left.Xyz, right.Xyz), left.W * right.W - Vector3d.Dot (left.Xyz, right.Xyz));
        }
        
        public static void Multiply (ref Quaterniond quaternion, double scale, out Quaterniond result)
        {
            result = new Quaterniond (quaternion.X * scale, quaternion.Y * scale, quaternion.Z * scale, quaternion.W * scale);
        }
        
        public static Quaterniond Multiply (Quaterniond quaternion, double scale)
        {
            return new Quaterniond (quaternion.X * scale, quaternion.Y * scale, quaternion.Z * scale, quaternion.W * scale);
        }
        
        public static void Multiply (ref Quaterniond left, ref Quaterniond right, out Quaterniond result)
        {
            result = new Quaterniond (right.W * left.Xyz + left.W * right.Xyz + Vector3d.Cross (left.Xyz, right.Xyz), left.W * right.W - Vector3d.Dot (left.Xyz, right.Xyz));
        }
        
        public static Quaterniond Multiply (Quaterniond left, Quaterniond right)
        {
            Quaterniond result;
            Quaterniond.Multiply (ref left, ref right, out result);
            return result;
        }
        
        public static void Normalize (ref Quaterniond q, out Quaterniond result)
        {
            double num = 1.0 / q.Length;
            result = new Quaterniond (q.Xyz * num, q.W * num);
        }
        
        public static Quaterniond Normalize (Quaterniond q)
        {
            Quaterniond result;
            Quaterniond.Normalize (ref q, out result);
            return result;
        }
        
        public static Quaterniond Slerp (Quaterniond q1, Quaterniond q2, double blend)
        {
            if (q1.LengthSquared == 0.0)
            {
                if (q2.LengthSquared == 0.0)
                {
                    return Quaterniond.Identity;
                }
                return q2;
            }
            else
            {
                if (q2.LengthSquared == 0.0)
                {
                    return q1;
                }
                double num = q1.W * q2.W + Vector3d.Dot (q1.Xyz, q2.Xyz);
                if (num >= 1.0 || num <= -1.0)
                {
                    return q1;
                }
                if (num < 0.0)
                {
                    q2.Xyz = -q2.Xyz;
                    q2.W = -q2.W;
                    num = -num;
                }
                double num5;
                double num6;
                if (num < 0.99000000953674316)
                {
                    double num2 = Math.Acos (num);
                    double num3 = Math.Sin (num2);
                    double num4 = 1.0 / num3;
                    num5 = Math.Sin (num2 * (1.0 - blend)) * num4;
                    num6 = Math.Sin (num2 * blend) * num4;
                }
                else
                {
                    num5 = 1.0 - blend;
                    num6 = blend;
                }
                Quaterniond q3 = new Quaterniond (num5 * q1.Xyz + num6 * q2.Xyz, num5 * q1.W + num6 * q2.W);
                if (q3.LengthSquared > 0.0)
                {
                    return Quaterniond.Normalize (q3);
                }
                return Quaterniond.Identity;
            }
        }
        
        public static Quaterniond Sub (Quaterniond left, Quaterniond right)
        {
            return new Quaterniond (left.Xyz - right.Xyz, left.W - right.W);
        }
        
        public static void Sub (ref Quaterniond left, ref Quaterniond right, out Quaterniond result)
        {
            result = new Quaterniond (left.Xyz - right.Xyz, left.W - right.W);
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
            return other is Quaterniond && this == (Quaterniond)other;
        }
        
        public bool Equals (Quaterniond other)
        {
            return this.Xyz == other.Xyz && this.W == other.W;
        }
        
        public override int GetHashCode ()
        {
            return this.Xyz.GetHashCode () ^ this.W.GetHashCode ();
        }
        
        public void Normalize ()
        {
            double num = 1.0 / this.Length;
            this.Xyz *= num;
            this.W *= num;
        }
        
        public Vector4d ToAxisAngle ()
        {
            Quaterniond quaterniond = this;
            if (quaterniond.W > 1.0)
            {
                quaterniond.Normalize ();
            }
            Vector4d result = default(Vector4d);
            result.W = (double)(2f * (float)Math.Acos (quaterniond.W));
            float num = (float)Math.Sqrt (1.0 - quaterniond.W * quaterniond.W);
            if (num > 0.0001f)
            {
                result.Xyz = quaterniond.Xyz / (double)num;
            }
            else
            {
                result.Xyz = Vector3d.UnitX;
            }
            return result;
        }
        
        public void ToAxisAngle (out Vector3d axis, out double angle)
        {
            Vector4d vector4d = this.ToAxisAngle ();
            axis = vector4d.Xyz;
            angle = vector4d.W;
        }
        
        public override string ToString ()
        {
            return string.Format ("V: {0}, W: {1}", this.Xyz, this.W);
        }
        
        //
        // Operators
        //
        
        public static Quaterniond operator + (Quaterniond left, Quaterniond right)
        {
            left.Xyz += right.Xyz;
            left.W += right.W;
            return left;
        }
        
        public static bool operator == (Quaterniond left, Quaterniond right)
        {
            return left.Equals (right);
        }
        
        public static bool operator != (Quaterniond left, Quaterniond right)
        {
            return !left.Equals (right);
        }
        
        public static Quaterniond operator * (Quaterniond left, Quaterniond right)
        {
            Quaterniond.Multiply (ref left, ref right, out left);
            return left;
        }
        
        public static Quaterniond operator * (double scale, Quaterniond quaternion)
        {
            return new Quaterniond (quaternion.X * scale, quaternion.Y * scale, quaternion.Z * scale, quaternion.W * scale);
        }
        
        public static Quaterniond operator * (Quaterniond quaternion, double scale)
        {
            Quaterniond.Multiply (ref quaternion, scale, out quaternion);
            return quaternion;
        }
        
        public static Quaterniond operator - (Quaterniond left, Quaterniond right)
        {
            left.Xyz -= right.Xyz;
            left.W -= right.W;
            return left;
        }
    }
}
