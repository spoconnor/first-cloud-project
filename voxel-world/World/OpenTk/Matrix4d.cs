using System;
namespace OpenTK
{
    public struct Matrix4d : IEquatable<Matrix4d>
    {
        //
        // Static Fields
        //
        
        public static Matrix4d Identity;
        
        //
        // Fields
        //
        
        public Vector4d Row0;
        
        public Vector4d Row3;
        
        public Vector4d Row2;
        
        public Vector4d Row1;
        
        //
        // Properties
        //
        
        public Vector4d Column0
        {
            get
            {
                return new Vector4d (this.Row0.X, this.Row1.X, this.Row2.X, this.Row3.X);
            }
        }
        
        public Vector4d Column1
        {
            get
            {
                return new Vector4d (this.Row0.Y, this.Row1.Y, this.Row2.Y, this.Row3.Y);
            }
        }
        
        public Vector4d Column2
        {
            get
            {
                return new Vector4d (this.Row0.Z, this.Row1.Z, this.Row2.Z, this.Row3.Z);
            }
        }
        
        public Vector4d Column3
        {
            get
            {
                return new Vector4d (this.Row0.W, this.Row1.W, this.Row2.W, this.Row3.W);
            }
        }
        
        public double Determinant
        {
            get
            {
                return this.Row0.X * this.Row1.Y * this.Row2.Z * this.Row3.W - this.Row0.X * this.Row1.Y * this.Row2.W * this.Row3.Z + this.Row0.X * this.Row1.Z * this.Row2.W * this.Row3.Y - this.Row0.X * this.Row1.Z * this.Row2.Y * this.Row3.W + this.Row0.X * this.Row1.W * this.Row2.Y * this.Row3.Z - this.Row0.X * this.Row1.W * this.Row2.Z * this.Row3.Y - this.Row0.Y * this.Row1.Z * this.Row2.W * this.Row3.X + this.Row0.Y * this.Row1.Z * this.Row2.X * this.Row3.W - this.Row0.Y * this.Row1.W * this.Row2.X * this.Row3.Z + this.Row0.Y * this.Row1.W * this.Row2.Z * this.Row3.X - this.Row0.Y * this.Row1.X * this.Row2.Z * this.Row3.W + this.Row0.Y * this.Row1.X * this.Row2.W * this.Row3.Z + this.Row0.Z * this.Row1.W * this.Row2.X * this.Row3.Y - this.Row0.Z * this.Row1.W * this.Row2.Y * this.Row3.X + this.Row0.Z * this.Row1.X * this.Row2.Y * this.Row3.W - this.Row0.Z * this.Row1.X * this.Row2.W * this.Row3.Y + this.Row0.Z * this.Row1.Y * this.Row2.W * this.Row3.X - this.Row0.Z * this.Row1.Y * this.Row2.X * this.Row3.W - this.Row0.W * this.Row1.X * this.Row2.Y * this.Row3.Z + this.Row0.W * this.Row1.X * this.Row2.Z * this.Row3.Y - this.Row0.W * this.Row1.Y * this.Row2.Z * this.Row3.X + this.Row0.W * this.Row1.Y * this.Row2.X * this.Row3.Z - this.Row0.W * this.Row1.Z * this.Row2.X * this.Row3.Y + this.Row0.W * this.Row1.Z * this.Row2.Y * this.Row3.X;
            }
        }
        
        public double M11
        {
            get
            {
                return this.Row0.X;
            }
            set
            {
                this.Row0.X = value;
            }
        }
        
        public double M12
        {
            get
            {
                return this.Row0.Y;
            }
            set
            {
                this.Row0.Y = value;
            }
        }
        
        public double M13
        {
            get
            {
                return this.Row0.Z;
            }
            set
            {
                this.Row0.Z = value;
            }
        }
        
        public double M14
        {
            get
            {
                return this.Row0.W;
            }
            set
            {
                this.Row0.W = value;
            }
        }
        
        public double M21
        {
            get
            {
                return this.Row1.X;
            }
            set
            {
                this.Row1.X = value;
            }
        }
        
        public double M22
        {
            get
            {
                return this.Row1.Y;
            }
            set
            {
                this.Row1.Y = value;
            }
        }
        
        public double M23
        {
            get
            {
                return this.Row1.Z;
            }
            set
            {
                this.Row1.Z = value;
            }
        }
        
        public double M24
        {
            get
            {
                return this.Row1.W;
            }
            set
            {
                this.Row1.W = value;
            }
        }
        
        public double M31
        {
            get
            {
                return this.Row2.X;
            }
            set
            {
                this.Row2.X = value;
            }
        }
        
        public double M32
        {
            get
            {
                return this.Row2.Y;
            }
            set
            {
                this.Row2.Y = value;
            }
        }
        
        public double M33
        {
            get
            {
                return this.Row2.Z;
            }
            set
            {
                this.Row2.Z = value;
            }
        }
        
        public double M34
        {
            get
            {
                return this.Row2.W;
            }
            set
            {
                this.Row2.W = value;
            }
        }
        
        public double M41
        {
            get
            {
                return this.Row3.X;
            }
            set
            {
                this.Row3.X = value;
            }
        }
        
        public double M42
        {
            get
            {
                return this.Row3.Y;
            }
            set
            {
                this.Row3.Y = value;
            }
        }
        
        public double M43
        {
            get
            {
                return this.Row3.Z;
            }
            set
            {
                this.Row3.Z = value;
            }
        }
        
        public double M44
        {
            get
            {
                return this.Row3.W;
            }
            set
            {
                this.Row3.W = value;
            }
        }
        
        //
        // Constructors
        //
        
        public Matrix4d (Vector4d row0, Vector4d row1, Vector4d row2, Vector4d row3)
        {
            this.Row0 = row0;
            this.Row1 = row1;
            this.Row2 = row2;
            this.Row3 = row3;
        }
        
        public Matrix4d (double m00, double m01, double m02, double m03, double m10, double m11, double m12, double m13, double m20, double m21, double m22, double m23, double m30, double m31, double m32, double m33)
        {
            this.Row0 = new Vector4d (m00, m01, m02, m03);
            this.Row1 = new Vector4d (m10, m11, m12, m13);
            this.Row2 = new Vector4d (m20, m21, m22, m23);
            this.Row3 = new Vector4d (m30, m31, m32, m33);
        }
        
        //
        // Static Methods
        //
        
        public static Matrix4d CreateFromAxisAngle (Vector3d axis, double angle)
        {
            Matrix4d result;
            Matrix4d.CreateFromAxisAngle (axis, angle, out result);
            return result;
        }
        
        public static void CreateFromAxisAngle (Vector3d axis, double angle, out Matrix4d result)
        {
            double num = Math.Cos (-angle);
            double num2 = Math.Sin (-angle);
            double num3 = 1.0 - num;
            axis.Normalize ();
            result = new Matrix4d (num3 * axis.X * axis.X + num, num3 * axis.X * axis.Y - num2 * axis.Z, num3 * axis.X * axis.Z + num2 * axis.Y, 0.0, num3 * axis.X * axis.Y + num2 * axis.Z, num3 * axis.Y * axis.Y + num, num3 * axis.Y * axis.Z - num2 * axis.X, 0.0, num3 * axis.X * axis.Z - num2 * axis.Y, num3 * axis.Y * axis.Z + num2 * axis.X, num3 * axis.Z * axis.Z + num, 0.0, 0.0, 0.0, 0.0, 1.0);
        }
        
        public static Matrix4d CreateOrthographic (double width, double height, double zNear, double zFar)
        {
            Matrix4d result;
            Matrix4d.CreateOrthographicOffCenter (-width / 2.0, width / 2.0, -height / 2.0, height / 2.0, zNear, zFar, out result);
            return result;
        }
        
        public static void CreateOrthographic (double width, double height, double zNear, double zFar, out Matrix4d result)
        {
            Matrix4d.CreateOrthographicOffCenter (-width / 2.0, width / 2.0, -height / 2.0, height / 2.0, zNear, zFar, out result);
        }
        
        public static void CreateOrthographicOffCenter (double left, double right, double bottom, double top, double zNear, double zFar, out Matrix4d result)
        {
            result = default(Matrix4d);
            double num = 1.0 / (right - left);
            double num2 = 1.0 / (top - bottom);
            double num3 = 1.0 / (zFar - zNear);
            result.M11 = 2.0 * num;
            result.M22 = 2.0 * num2;
            result.M33 = -2.0 * num3;
            result.M41 = -(right + left) * num;
            result.M42 = -(top + bottom) * num2;
            result.M43 = -(zFar + zNear) * num3;
            result.M44 = 1.0;
        }
        
        public static Matrix4d CreateOrthographicOffCenter (double left, double right, double bottom, double top, double zNear, double zFar)
        {
            Matrix4d result;
            Matrix4d.CreateOrthographicOffCenter (left, right, bottom, top, zNear, zFar, out result);
            return result;
        }
        
        public static Matrix4d CreatePerspectiveFieldOfView (double fovy, double aspect, double zNear, double zFar)
        {
            Matrix4d result;
            Matrix4d.CreatePerspectiveFieldOfView (fovy, aspect, zNear, zFar, out result);
            return result;
        }
        
        public static void CreatePerspectiveFieldOfView (double fovy, double aspect, double zNear, double zFar, out Matrix4d result)
        {
            if (fovy <= 0.0 || fovy > 3.1415926535897931)
            {
                throw new ArgumentOutOfRangeException ("fovy");
            }
            if (aspect <= 0.0)
            {
                throw new ArgumentOutOfRangeException ("aspect");
            }
            if (zNear <= 0.0)
            {
                throw new ArgumentOutOfRangeException ("zNear");
            }
            if (zFar <= 0.0)
            {
                throw new ArgumentOutOfRangeException ("zFar");
            }
            if (zNear >= zFar)
            {
                throw new ArgumentOutOfRangeException ("zNear");
            }
            double num = zNear * Math.Tan (0.5 * fovy);
            double num2 = -num;
            double left = num2 * aspect;
            double right = num * aspect;
            Matrix4d.CreatePerspectiveOffCenter (left, right, num2, num, zNear, zFar, out result);
        }
        
        public static void CreatePerspectiveOffCenter (double left, double right, double bottom, double top, double zNear, double zFar, out Matrix4d result)
        {
            if (zNear <= 0.0)
            {
                throw new ArgumentOutOfRangeException ("zNear");
            }
            if (zFar <= 0.0)
            {
                throw new ArgumentOutOfRangeException ("zFar");
            }
            if (zNear >= zFar)
            {
                throw new ArgumentOutOfRangeException ("zNear");
            }
            double m = 2.0 * zNear / (right - left);
            double m2 = 2.0 * zNear / (top - bottom);
            double m3 = (right + left) / (right - left);
            double m4 = (top + bottom) / (top - bottom);
            double m5 = -(zFar + zNear) / (zFar - zNear);
            double m6 = -(2.0 * zFar * zNear) / (zFar - zNear);
            result = new Matrix4d (m, 0.0, 0.0, 0.0, 0.0, m2, 0.0, 0.0, m3, m4, m5, -1.0, 0.0, 0.0, m6, 0.0);
        }
        
        public static Matrix4d CreatePerspectiveOffCenter (double left, double right, double bottom, double top, double zNear, double zFar)
        {
            Matrix4d result;
            Matrix4d.CreatePerspectiveOffCenter (left, right, bottom, top, zNear, zFar, out result);
            return result;
        }
        
        public static void CreateRotationX (double angle, out Matrix4d result)
        {
            double num = Math.Cos (angle);
            double num2 = Math.Sin (angle);
            result.Row0 = Vector4d.UnitX;
            result.Row1 = new Vector4d (0.0, num, num2, 0.0);
            result.Row2 = new Vector4d (0.0, -num2, num, 0.0);
            result.Row3 = Vector4d.UnitW;
        }
        
        public static Matrix4d CreateRotationX (double angle)
        {
            Matrix4d result;
            Matrix4d.CreateRotationX (angle, out result);
            return result;
        }
        
        public static void CreateRotationY (double angle, out Matrix4d result)
        {
            double num = Math.Cos (angle);
            double num2 = Math.Sin (angle);
            result.Row0 = new Vector4d (num, 0.0, -num2, 0.0);
            result.Row1 = Vector4d.UnitY;
            result.Row2 = new Vector4d (num2, 0.0, num, 0.0);
            result.Row3 = Vector4d.UnitW;
        }
        
        public static Matrix4d CreateRotationY (double angle)
        {
            Matrix4d result;
            Matrix4d.CreateRotationY (angle, out result);
            return result;
        }
        
        public static void CreateRotationZ (double angle, out Matrix4d result)
        {
            double num = Math.Cos (angle);
            double num2 = Math.Sin (angle);
            result.Row0 = new Vector4d (num, num2, 0.0, 0.0);
            result.Row1 = new Vector4d (-num2, num, 0.0, 0.0);
            result.Row2 = Vector4d.UnitZ;
            result.Row3 = Vector4d.UnitW;
        }
        
        public static Matrix4d CreateRotationZ (double angle)
        {
            Matrix4d result;
            Matrix4d.CreateRotationZ (angle, out result);
            return result;
        }
        
        public static void CreateTranslation (ref Vector3d vector, out Matrix4d result)
        {
            result = Matrix4d.Identity;
            result.Row3 = new Vector4d (vector.X, vector.Y, vector.Z, 1.0);
        }
        
        public static Matrix4d CreateTranslation (Vector3d vector)
        {
            Matrix4d result;
            Matrix4d.CreateTranslation (vector.X, vector.Y, vector.Z, out result);
            return result;
        }
        
        public static Matrix4d CreateTranslation (double x, double y, double z)
        {
            Matrix4d result;
            Matrix4d.CreateTranslation (x, y, z, out result);
            return result;
        }
        
        public static void CreateTranslation (double x, double y, double z, out Matrix4d result)
        {
            result = Matrix4d.Identity;
            result.Row3 = new Vector4d (x, y, z, 1.0);
        }
        
        public static Matrix4d Frustum (double left, double right, double bottom, double top, double near, double far)
        {
            double num = 1.0 / (right - left);
            double num2 = 1.0 / (top - bottom);
            double num3 = 1.0 / (far - near);
            return new Matrix4d (new Vector4d (2.0 * near * num, 0.0, 0.0, 0.0), new Vector4d (0.0, 2.0 * near * num2, 0.0, 0.0), new Vector4d ((right + left) * num, (top + bottom) * num2, -(far + near) * num3, -1.0), new Vector4d (0.0, 0.0, -2.0 * far * near * num3, 0.0));
        }
        
        public static Matrix4d Invert (Matrix4d mat)
        {
            int[] array = new int[4];
            int[] array2 = array;
            int[] array3 = new int[4];
            int[] array4 = array3;
            int[] array5 = new int[]
            {
                -1,
                -1,
                -1,
                -1
            };
            double[,] array6 = new double[4, 4];
            array6 [0, 0] = mat.Row0.X;
            array6 [0, 1] = mat.Row0.Y;
            array6 [0, 2] = mat.Row0.Z;
            array6 [0, 3] = mat.Row0.W;
            array6 [1, 0] = mat.Row1.X;
            array6 [1, 1] = mat.Row1.Y;
            array6 [1, 2] = mat.Row1.Z;
            array6 [1, 3] = mat.Row1.W;
            array6 [2, 0] = mat.Row2.X;
            array6 [2, 1] = mat.Row2.Y;
            array6 [2, 2] = mat.Row2.Z;
            array6 [2, 3] = mat.Row2.W;
            array6 [3, 0] = mat.Row3.X;
            array6 [3, 1] = mat.Row3.Y;
            array6 [3, 2] = mat.Row3.Z;
            array6 [3, 3] = mat.Row3.W;
            double[,] array7 = array6;
            int num = 0;
            int num2 = 0;
            for (int i = 0; i < 4; i++)
            {
                double num3 = 0.0;
                for (int j = 0; j < 4; j++)
                {
                    if (array5 [j] != 0)
                    {
                        for (int k = 0; k < 4; k++)
                        {
                            if (array5 [k] == -1)
                            {
                                double num4 = Math.Abs (array7 [j, k]);
                                if (num4 > num3)
                                {
                                    num3 = num4;
                                    num2 = j;
                                    num = k;
                                }
                            }
                            else
                            {
                                if (array5 [k] > 0)
                                {
                                    return mat;
                                }
                            }
                        }
                    }
                }
                array5 [num]++;
                if (num2 != num)
                {
                    for (int l = 0; l < 4; l++)
                    {
                        double num5 = array7 [num2, l];
                        array7 [num2, l] = array7 [num, l];
                        array7 [num, l] = num5;
                    }
                }
                array4 [i] = num2;
                array2 [i] = num;
                double num6 = array7 [num, num];
                if (num6 == 0.0)
                {
                    throw new InvalidOperationException ("Matrix is singular and cannot be inverted.");
                }
                double num7 = 1.0 / num6;
                array7 [num, num] = 1.0;
                for (int m = 0; m < 4; m++)
                {
                    array7 [num, m] *= num7;
                }
                for (int n = 0; n < 4; n++)
                {
                    if (num != n)
                    {
                        double num8 = array7 [n, num];
                        array7 [n, num] = 0.0;
                        for (int num9 = 0; num9 < 4; num9++)
                        {
                            array7 [n, num9] -= array7 [num, num9] * num8;
                        }
                    }
                }
            }
            for (int num10 = 3; num10 >= 0; num10--)
            {
                int num11 = array4 [num10];
                int num12 = array2 [num10];
                for (int num13 = 0; num13 < 4; num13++)
                {
                    double num14 = array7 [num13, num11];
                    array7 [num13, num11] = array7 [num13, num12];
                    array7 [num13, num12] = num14;
                }
            }
            mat.Row0 = new Vector4d (array7 [0, 0], array7 [0, 1], array7 [0, 2], array7 [0, 3]);
            mat.Row1 = new Vector4d (array7 [1, 0], array7 [1, 1], array7 [1, 2], array7 [1, 3]);
            mat.Row2 = new Vector4d (array7 [2, 0], array7 [2, 1], array7 [2, 2], array7 [2, 3]);
            mat.Row3 = new Vector4d (array7 [3, 0], array7 [3, 1], array7 [3, 2], array7 [3, 3]);
            return mat;
        }
        
        public static Matrix4d LookAt (Vector3d eye, Vector3d target, Vector3d up)
        {
            Vector3d vector3d = Vector3d.Normalize (eye - target);
            Vector3d right = Vector3d.Normalize (Vector3d.Cross (up, vector3d));
            Vector3d vector3d2 = Vector3d.Normalize (Vector3d.Cross (vector3d, right));
            Matrix4d right2 = new Matrix4d (new Vector4d (right.X, vector3d2.X, vector3d.X, 0.0), new Vector4d (right.Y, vector3d2.Y, vector3d.Y, 0.0), new Vector4d (right.Z, vector3d2.Z, vector3d.Z, 0.0), Vector4d.UnitW);
            Matrix4d left = Matrix4d.CreateTranslation (-eye);
            return left * right2;
        }
        
        public static Matrix4d LookAt (double eyeX, double eyeY, double eyeZ, double targetX, double targetY, double targetZ, double upX, double upY, double upZ)
        {
            return Matrix4d.LookAt (new Vector3d (eyeX, eyeY, eyeZ), new Vector3d (targetX, targetY, targetZ), new Vector3d (upX, upY, upZ));
        }
        
        public static Matrix4d Mult (Matrix4d left, Matrix4d right)
        {
            Matrix4d result;
            Matrix4d.Mult (ref left, ref right, out result);
            return result;
        }
        
        public static void Mult (ref Matrix4d left, ref Matrix4d right, out Matrix4d result)
        {
            result = default(Matrix4d);
            result.M11 = left.M11 * right.M11 + left.M12 * right.M21 + left.M13 * right.M31 + left.M14 * right.M41;
            result.M12 = left.M11 * right.M12 + left.M12 * right.M22 + left.M13 * right.M32 + left.M14 * right.M42;
            result.M13 = left.M11 * right.M13 + left.M12 * right.M23 + left.M13 * right.M33 + left.M14 * right.M43;
            result.M14 = left.M11 * right.M14 + left.M12 * right.M24 + left.M13 * right.M34 + left.M14 * right.M44;
            result.M21 = left.M21 * right.M11 + left.M22 * right.M21 + left.M23 * right.M31 + left.M24 * right.M41;
            result.M22 = left.M21 * right.M12 + left.M22 * right.M22 + left.M23 * right.M32 + left.M24 * right.M42;
            result.M23 = left.M21 * right.M13 + left.M22 * right.M23 + left.M23 * right.M33 + left.M24 * right.M43;
            result.M24 = left.M21 * right.M14 + left.M22 * right.M24 + left.M23 * right.M34 + left.M24 * right.M44;
            result.M31 = left.M31 * right.M11 + left.M32 * right.M21 + left.M33 * right.M31 + left.M34 * right.M41;
            result.M32 = left.M31 * right.M12 + left.M32 * right.M22 + left.M33 * right.M32 + left.M34 * right.M42;
            result.M33 = left.M31 * right.M13 + left.M32 * right.M23 + left.M33 * right.M33 + left.M34 * right.M43;
            result.M34 = left.M31 * right.M14 + left.M32 * right.M24 + left.M33 * right.M34 + left.M34 * right.M44;
            result.M41 = left.M41 * right.M11 + left.M42 * right.M21 + left.M43 * right.M31 + left.M44 * right.M41;
            result.M42 = left.M41 * right.M12 + left.M42 * right.M22 + left.M43 * right.M32 + left.M44 * right.M42;
            result.M43 = left.M41 * right.M13 + left.M42 * right.M23 + left.M43 * right.M33 + left.M44 * right.M43;
            result.M44 = left.M41 * right.M14 + left.M42 * right.M24 + left.M43 * right.M34 + left.M44 * right.M44;
        }
        
        public static Matrix4d Perspective (double fovy, double aspect, double near, double far)
        {
            double num = near * Math.Tan (0.5 * fovy);
            double num2 = -num;
            double left = num2 * aspect;
            double right = num * aspect;
            return Matrix4d.Frustum (left, right, num2, num, near, far);
        }
        
        public static Matrix4d Rotate (Quaterniond q)
        {
            Vector3d axis;
            double angle;
            q.ToAxisAngle (out axis, out angle);
            return Matrix4d.Rotate (axis, angle);
        }
        
        public static Matrix4d Rotate (Vector3d axis, double angle)
        {
            double num = Math.Cos (-angle);
            double num2 = Math.Sin (-angle);
            double num3 = 1.0 - num;
            axis.Normalize ();
            Matrix4d result;
            result.Row0 = new Vector4d (num3 * axis.X * axis.X + num, num3 * axis.X * axis.Y - num2 * axis.Z, num3 * axis.X * axis.Z + num2 * axis.Y, 0.0);
            result.Row1 = new Vector4d (num3 * axis.X * axis.Y + num2 * axis.Z, num3 * axis.Y * axis.Y + num, num3 * axis.Y * axis.Z - num2 * axis.X, 0.0);
            result.Row2 = new Vector4d (num3 * axis.X * axis.Z - num2 * axis.Y, num3 * axis.Y * axis.Z + num2 * axis.X, num3 * axis.Z * axis.Z + num, 0.0);
            result.Row3 = Vector4d.UnitW;
            return result;
        }
        
        public static Matrix4d RotateX (double angle)
        {
            double num = Math.Cos (angle);
            double num2 = Math.Sin (angle);
            Matrix4d result;
            result.Row0 = Vector4d.UnitX;
            result.Row1 = new Vector4d (0.0, num, num2, 0.0);
            result.Row2 = new Vector4d (0.0, -num2, num, 0.0);
            result.Row3 = Vector4d.UnitW;
            return result;
        }
        
        public static Matrix4d RotateY (double angle)
        {
            double num = Math.Cos (angle);
            double num2 = Math.Sin (angle);
            Matrix4d result;
            result.Row0 = new Vector4d (num, 0.0, -num2, 0.0);
            result.Row1 = Vector4d.UnitY;
            result.Row2 = new Vector4d (num2, 0.0, num, 0.0);
            result.Row3 = Vector4d.UnitW;
            return result;
        }
        
        public static Matrix4d RotateZ (double angle)
        {
            double num = Math.Cos (angle);
            double num2 = Math.Sin (angle);
            Matrix4d result;
            result.Row0 = new Vector4d (num, num2, 0.0, 0.0);
            result.Row1 = new Vector4d (-num2, num, 0.0, 0.0);
            result.Row2 = Vector4d.UnitZ;
            result.Row3 = Vector4d.UnitW;
            return result;
        }
        
        public static Matrix4d Scale (double scale)
        {
            return Matrix4d.Scale (scale, scale, scale);
        }
        
        public static Matrix4d Scale (Vector3d scale)
        {
            return Matrix4d.Scale (scale.X, scale.Y, scale.Z);
        }
        
        public static Matrix4d Scale (double x, double y, double z)
        {
            Matrix4d result;
            result.Row0 = Vector4d.UnitX * x;
            result.Row1 = Vector4d.UnitY * y;
            result.Row2 = Vector4d.UnitZ * z;
            result.Row3 = Vector4d.UnitW;
            return result;
        }
        
        [Obsolete ("Use CreateTranslation instead.")]
        public static Matrix4d Translation (double x, double y, double z)
        {
            Matrix4d identity = Matrix4d.Identity;
            identity.Row3 = new Vector4d (x, y, z, 1.0);
            return identity;
        }
        
        [Obsolete ("Use CreateTranslation instead.")]
        public static Matrix4d Translation (Vector3d trans)
        {
            return Matrix4d.Translation (trans.X, trans.Y, trans.Z);
        }
        
        public static void Transpose (ref Matrix4d mat, out Matrix4d result)
        {
            result.Row0 = mat.Column0;
            result.Row1 = mat.Column1;
            result.Row2 = mat.Column2;
            result.Row3 = mat.Column3;
        }
        
        public static Matrix4d Transpose (Matrix4d mat)
        {
            return new Matrix4d (mat.Column0, mat.Column1, mat.Column2, mat.Column3);
        }
        
        //
        // Methods
        //
        
        public override bool Equals (object obj)
        {
            return obj is Matrix4d && this.Equals ((Matrix4d)obj);
        }
        
        public bool Equals (Matrix4d other)
        {
            return this.Row0 == other.Row0 && this.Row1 == other.Row1 && this.Row2 == other.Row2 && this.Row3 == other.Row3;
        }
        
        public override int GetHashCode ()
        {
            return this.Row0.GetHashCode () ^ this.Row1.GetHashCode () ^ this.Row2.GetHashCode () ^ this.Row3.GetHashCode ();
        }
        
        public void Invert ()
        {
            this = Matrix4d.Invert (this);
        }
        
        public override string ToString ()
        {
            return string.Format ("{0}\n{1}\n{2}\n{3}", new object[]
                                  {
                this.Row0,
                this.Row1,
                this.Row2,
                this.Row3
            });
        }
        
        public void Transpose ()
        {
            this = Matrix4d.Transpose (this);
        }
        
        //
        // Operators
        //
        
        public static bool operator == (Matrix4d left, Matrix4d right)
        {
            return left.Equals (right);
        }
        
        public static bool operator != (Matrix4d left, Matrix4d right)
        {
            return !left.Equals (right);
        }
        
        public static Matrix4d operator * (Matrix4d left, Matrix4d right)
        {
            return Matrix4d.Mult (left, right);
        }
    }
}
