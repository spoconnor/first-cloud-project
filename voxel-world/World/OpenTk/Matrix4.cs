using System;
namespace OpenTK
{
    public struct Matrix4 : IEquatable<Matrix4>
    {
        //
        // Static Fields
        //
        
        public static Matrix4 Identity;
        
        //
        // Fields
        //
        
        public Vector4 Row0;
        
        public Vector4 Row3;
        
        public Vector4 Row2;
        
        public Vector4 Row1;
        
        //
        // Properties
        //
        
        public Vector4 Column0
        {
            get
            {
                return new Vector4 (this.Row0.X, this.Row1.X, this.Row2.X, this.Row3.X);
            }
        }
        
        public Vector4 Column1
        {
            get
            {
                return new Vector4 (this.Row0.Y, this.Row1.Y, this.Row2.Y, this.Row3.Y);
            }
        }
        
        public Vector4 Column2
        {
            get
            {
                return new Vector4 (this.Row0.Z, this.Row1.Z, this.Row2.Z, this.Row3.Z);
            }
        }
        
        public Vector4 Column3
        {
            get
            {
                return new Vector4 (this.Row0.W, this.Row1.W, this.Row2.W, this.Row3.W);
            }
        }
        
        public float Determinant
        {
            get
            {
                return this.Row0.X * this.Row1.Y * this.Row2.Z * this.Row3.W - this.Row0.X * this.Row1.Y * this.Row2.W * this.Row3.Z + this.Row0.X * this.Row1.Z * this.Row2.W * this.Row3.Y - this.Row0.X * this.Row1.Z * this.Row2.Y * this.Row3.W + this.Row0.X * this.Row1.W * this.Row2.Y * this.Row3.Z - this.Row0.X * this.Row1.W * this.Row2.Z * this.Row3.Y - this.Row0.Y * this.Row1.Z * this.Row2.W * this.Row3.X + this.Row0.Y * this.Row1.Z * this.Row2.X * this.Row3.W - this.Row0.Y * this.Row1.W * this.Row2.X * this.Row3.Z + this.Row0.Y * this.Row1.W * this.Row2.Z * this.Row3.X - this.Row0.Y * this.Row1.X * this.Row2.Z * this.Row3.W + this.Row0.Y * this.Row1.X * this.Row2.W * this.Row3.Z + this.Row0.Z * this.Row1.W * this.Row2.X * this.Row3.Y - this.Row0.Z * this.Row1.W * this.Row2.Y * this.Row3.X + this.Row0.Z * this.Row1.X * this.Row2.Y * this.Row3.W - this.Row0.Z * this.Row1.X * this.Row2.W * this.Row3.Y + this.Row0.Z * this.Row1.Y * this.Row2.W * this.Row3.X - this.Row0.Z * this.Row1.Y * this.Row2.X * this.Row3.W - this.Row0.W * this.Row1.X * this.Row2.Y * this.Row3.Z + this.Row0.W * this.Row1.X * this.Row2.Z * this.Row3.Y - this.Row0.W * this.Row1.Y * this.Row2.Z * this.Row3.X + this.Row0.W * this.Row1.Y * this.Row2.X * this.Row3.Z - this.Row0.W * this.Row1.Z * this.Row2.X * this.Row3.Y + this.Row0.W * this.Row1.Z * this.Row2.Y * this.Row3.X;
            }
        }
        
        public float M11
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
        
        public float M12
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
        
        public float M13
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
        
        public float M14
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
        
        public float M21
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
        
        public float M22
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
        
        public float M23
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
        
        public float M24
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
        
        public float M31
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
        
        public float M32
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
        
        public float M33
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
        
        public float M34
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
        
        public float M41
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
        
        public float M42
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
        
        public float M43
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
        
        public float M44
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
        
        public Matrix4 (Vector4 row0, Vector4 row1, Vector4 row2, Vector4 row3)
        {
            this.Row0 = row0;
            this.Row1 = row1;
            this.Row2 = row2;
            this.Row3 = row3;
        }
        
        public Matrix4 (float m00, float m01, float m02, float m03, float m10, float m11, float m12, float m13, float m20, float m21, float m22, float m23, float m30, float m31, float m32, float m33)
        {
            this.Row0 = new Vector4 (m00, m01, m02, m03);
            this.Row1 = new Vector4 (m10, m11, m12, m13);
            this.Row2 = new Vector4 (m20, m21, m22, m23);
            this.Row3 = new Vector4 (m30, m31, m32, m33);
        }
        
        //
        // Static Methods
        //
        
        public static Matrix4 CreateFromAxisAngle (Vector3 axis, float angle)
        {
            Matrix4 result;
            Matrix4.CreateFromAxisAngle (axis, angle, out result);
            return result;
        }
        
        public static void CreateFromAxisAngle (Vector3 axis, float angle, out Matrix4 result)
        {
            float num = (float)Math.Cos ((double)(-(double)angle));
            float num2 = (float)Math.Sin ((double)(-(double)angle));
            float num3 = 1f - num;
            axis.Normalize ();
            result = new Matrix4 (num3 * axis.X * axis.X + num, num3 * axis.X * axis.Y - num2 * axis.Z, num3 * axis.X * axis.Z + num2 * axis.Y, 0f, num3 * axis.X * axis.Y + num2 * axis.Z, num3 * axis.Y * axis.Y + num, num3 * axis.Y * axis.Z - num2 * axis.X, 0f, num3 * axis.X * axis.Z - num2 * axis.Y, num3 * axis.Y * axis.Z + num2 * axis.X, num3 * axis.Z * axis.Z + num, 0f, 0f, 0f, 0f, 1f);
        }
        
        public static Matrix4 CreateOrthographic (float width, float height, float zNear, float zFar)
        {
            Matrix4 result;
            Matrix4.CreateOrthographicOffCenter (-width / 2f, width / 2f, -height / 2f, height / 2f, zNear, zFar, out result);
            return result;
        }
        
        public static void CreateOrthographic (float width, float height, float zNear, float zFar, out Matrix4 result)
        {
            Matrix4.CreateOrthographicOffCenter (-width / 2f, width / 2f, -height / 2f, height / 2f, zNear, zFar, out result);
        }
        
        public static void CreateOrthographicOffCenter (float left, float right, float bottom, float top, float zNear, float zFar, out Matrix4 result)
        {
            result = default(Matrix4);
            float num = 1f / (right - left);
            float num2 = 1f / (top - bottom);
            float num3 = 1f / (zFar - zNear);
            result.M11 = 2f * num;
            result.M22 = 2f * num2;
            result.M33 = -2f * num3;
            result.M41 = -(right + left) * num;
            result.M42 = -(top + bottom) * num2;
            result.M43 = -(zFar + zNear) * num3;
            result.M44 = 1f;
        }
        
        public static Matrix4 CreateOrthographicOffCenter (float left, float right, float bottom, float top, float zNear, float zFar)
        {
            Matrix4 result;
            Matrix4.CreateOrthographicOffCenter (left, right, bottom, top, zNear, zFar, out result);
            return result;
        }
        
        public static Matrix4 CreatePerspectiveFieldOfView (float fovy, float aspect, float zNear, float zFar)
        {
            Matrix4 result;
            Matrix4.CreatePerspectiveFieldOfView (fovy, aspect, zNear, zFar, out result);
            return result;
        }
        
        public static void CreatePerspectiveFieldOfView (float fovy, float aspect, float zNear, float zFar, out Matrix4 result)
        {
            if (fovy <= 0f || (double)fovy > 3.1415926535897931)
            {
                throw new ArgumentOutOfRangeException ("fovy");
            }
            if (aspect <= 0f)
            {
                throw new ArgumentOutOfRangeException ("aspect");
            }
            if (zNear <= 0f)
            {
                throw new ArgumentOutOfRangeException ("zNear");
            }
            if (zFar <= 0f)
            {
                throw new ArgumentOutOfRangeException ("zFar");
            }
            if (zNear >= zFar)
            {
                throw new ArgumentOutOfRangeException ("zNear");
            }
            float num = zNear * (float)Math.Tan ((double)(0.5f * fovy));
            float num2 = -num;
            float left = num2 * aspect;
            float right = num * aspect;
            Matrix4.CreatePerspectiveOffCenter (left, right, num2, num, zNear, zFar, out result);
        }
        
        public static void CreatePerspectiveOffCenter (float left, float right, float bottom, float top, float zNear, float zFar, out Matrix4 result)
        {
            if (zNear <= 0f)
            {
                throw new ArgumentOutOfRangeException ("zNear");
            }
            if (zFar <= 0f)
            {
                throw new ArgumentOutOfRangeException ("zFar");
            }
            if (zNear >= zFar)
            {
                throw new ArgumentOutOfRangeException ("zNear");
            }
            float m = 2f * zNear / (right - left);
            float m2 = 2f * zNear / (top - bottom);
            float m3 = (right + left) / (right - left);
            float m4 = (top + bottom) / (top - bottom);
            float m5 = -(zFar + zNear) / (zFar - zNear);
            float m6 = -(2f * zFar * zNear) / (zFar - zNear);
            result = new Matrix4 (m, 0f, 0f, 0f, 0f, m2, 0f, 0f, m3, m4, m5, -1f, 0f, 0f, m6, 0f);
        }
        
        public static Matrix4 CreatePerspectiveOffCenter (float left, float right, float bottom, float top, float zNear, float zFar)
        {
            Matrix4 result;
            Matrix4.CreatePerspectiveOffCenter (left, right, bottom, top, zNear, zFar, out result);
            return result;
        }
        
        public static void CreateRotationX (float angle, out Matrix4 result)
        {
            float num = (float)Math.Cos ((double)angle);
            float num2 = (float)Math.Sin ((double)angle);
            result.Row0 = Vector4.UnitX;
            result.Row1 = new Vector4 (0f, num, num2, 0f);
            result.Row2 = new Vector4 (0f, -num2, num, 0f);
            result.Row3 = Vector4.UnitW;
        }
        
        public static Matrix4 CreateRotationX (float angle)
        {
            Matrix4 result;
            Matrix4.CreateRotationX (angle, out result);
            return result;
        }
        
        public static void CreateRotationY (float angle, out Matrix4 result)
        {
            float num = (float)Math.Cos ((double)angle);
            float num2 = (float)Math.Sin ((double)angle);
            result.Row0 = new Vector4 (num, 0f, -num2, 0f);
            result.Row1 = Vector4.UnitY;
            result.Row2 = new Vector4 (num2, 0f, num, 0f);
            result.Row3 = Vector4.UnitW;
        }
        
        public static Matrix4 CreateRotationY (float angle)
        {
            Matrix4 result;
            Matrix4.CreateRotationY (angle, out result);
            return result;
        }
        
        public static void CreateRotationZ (float angle, out Matrix4 result)
        {
            float num = (float)Math.Cos ((double)angle);
            float num2 = (float)Math.Sin ((double)angle);
            result.Row0 = new Vector4 (num, num2, 0f, 0f);
            result.Row1 = new Vector4 (-num2, num, 0f, 0f);
            result.Row2 = Vector4.UnitZ;
            result.Row3 = Vector4.UnitW;
        }
        
        public static Matrix4 CreateRotationZ (float angle)
        {
            Matrix4 result;
            Matrix4.CreateRotationZ (angle, out result);
            return result;
        }
        
        public static void CreateTranslation (ref Vector3 vector, out Matrix4 result)
        {
            result = Matrix4.Identity;
            result.Row3 = new Vector4 (vector.X, vector.Y, vector.Z, 1f);
        }
        
        public static Matrix4 CreateTranslation (Vector3 vector)
        {
            Matrix4 result;
            Matrix4.CreateTranslation (vector.X, vector.Y, vector.Z, out result);
            return result;
        }
        
        public static Matrix4 CreateTranslation (float x, float y, float z)
        {
            Matrix4 result;
            Matrix4.CreateTranslation (x, y, z, out result);
            return result;
        }
        
        public static void CreateTranslation (float x, float y, float z, out Matrix4 result)
        {
            result = Matrix4.Identity;
            result.Row3 = new Vector4 (x, y, z, 1f);
        }
        
        [Obsolete ("Use CreatePerspectiveOffCenter instead.")]
        public static Matrix4 Frustum (float left, float right, float bottom, float top, float near, float far)
        {
            float num = 1f / (right - left);
            float num2 = 1f / (top - bottom);
            float num3 = 1f / (far - near);
            return new Matrix4 (new Vector4 (2f * near * num, 0f, 0f, 0f), new Vector4 (0f, 2f * near * num2, 0f, 0f), new Vector4 ((right + left) * num, (top + bottom) * num2, -(far + near) * num3, -1f), new Vector4 (0f, 0f, -2f * far * near * num3, 0f));
        }
        
        public static Matrix4 Invert (Matrix4 mat)
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
            float[,] array6 = new float[4, 4];
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
            float[,] array7 = array6;
            int num = 0;
            int num2 = 0;
            for (int i = 0; i < 4; i++)
            {
                float num3 = 0f;
                for (int j = 0; j < 4; j++)
                {
                    if (array5 [j] != 0)
                    {
                        for (int k = 0; k < 4; k++)
                        {
                            if (array5 [k] == -1)
                            {
                                float num4 = Math.Abs (array7 [j, k]);
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
                        float num5 = array7 [num2, l];
                        array7 [num2, l] = array7 [num, l];
                        array7 [num, l] = num5;
                    }
                }
                array4 [i] = num2;
                array2 [i] = num;
                float num6 = array7 [num, num];
                if (num6 == 0f)
                {
                    throw new InvalidOperationException ("Matrix is singular and cannot be inverted.");
                }
                float num7 = 1f / num6;
                array7 [num, num] = 1f;
                for (int m = 0; m < 4; m++)
                {
                    array7 [num, m] *= num7;
                }
                for (int n = 0; n < 4; n++)
                {
                    if (num != n)
                    {
                        float num8 = array7 [n, num];
                        array7 [n, num] = 0f;
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
                    float num14 = array7 [num13, num11];
                    array7 [num13, num11] = array7 [num13, num12];
                    array7 [num13, num12] = num14;
                }
            }
            mat.Row0 = new Vector4 (array7 [0, 0], array7 [0, 1], array7 [0, 2], array7 [0, 3]);
            mat.Row1 = new Vector4 (array7 [1, 0], array7 [1, 1], array7 [1, 2], array7 [1, 3]);
            mat.Row2 = new Vector4 (array7 [2, 0], array7 [2, 1], array7 [2, 2], array7 [2, 3]);
            mat.Row3 = new Vector4 (array7 [3, 0], array7 [3, 1], array7 [3, 2], array7 [3, 3]);
            return mat;
        }
        
        public static Matrix4 LookAt (Vector3 eye, Vector3 target, Vector3 up)
        {
            Vector3 vector = Vector3.Normalize (eye - target);
            Vector3 right = Vector3.Normalize (Vector3.Cross (up, vector));
            Vector3 vector2 = Vector3.Normalize (Vector3.Cross (vector, right));
            Matrix4 right2 = new Matrix4 (new Vector4 (right.X, vector2.X, vector.X, 0f), new Vector4 (right.Y, vector2.Y, vector.Y, 0f), new Vector4 (right.Z, vector2.Z, vector.Z, 0f), Vector4.UnitW);
            Matrix4 left = Matrix4.CreateTranslation (-eye);
            return left * right2;
        }
        
        public static Matrix4 LookAt (float eyeX, float eyeY, float eyeZ, float targetX, float targetY, float targetZ, float upX, float upY, float upZ)
        {
            return Matrix4.LookAt (new Vector3 (eyeX, eyeY, eyeZ), new Vector3 (targetX, targetY, targetZ), new Vector3 (upX, upY, upZ));
        }
        
        public static Matrix4 Mult (Matrix4 left, Matrix4 right)
        {
            Matrix4 result;
            Matrix4.Mult (ref left, ref right, out result);
            return result;
        }
        
        public static void Mult (ref Matrix4 left, ref Matrix4 right, out Matrix4 result)
        {
            result = new Matrix4 (left.M11 * right.M11 + left.M12 * right.M21 + left.M13 * right.M31 + left.M14 * right.M41, left.M11 * right.M12 + left.M12 * right.M22 + left.M13 * right.M32 + left.M14 * right.M42, left.M11 * right.M13 + left.M12 * right.M23 + left.M13 * right.M33 + left.M14 * right.M43, left.M11 * right.M14 + left.M12 * right.M24 + left.M13 * right.M34 + left.M14 * right.M44, left.M21 * right.M11 + left.M22 * right.M21 + left.M23 * right.M31 + left.M24 * right.M41, left.M21 * right.M12 + left.M22 * right.M22 + left.M23 * right.M32 + left.M24 * right.M42, left.M21 * right.M13 + left.M22 * right.M23 + left.M23 * right.M33 + left.M24 * right.M43, left.M21 * right.M14 + left.M22 * right.M24 + left.M23 * right.M34 + left.M24 * right.M44, left.M31 * right.M11 + left.M32 * right.M21 + left.M33 * right.M31 + left.M34 * right.M41, left.M31 * right.M12 + left.M32 * right.M22 + left.M33 * right.M32 + left.M34 * right.M42, left.M31 * right.M13 + left.M32 * right.M23 + left.M33 * right.M33 + left.M34 * right.M43, left.M31 * right.M14 + left.M32 * right.M24 + left.M33 * right.M34 + left.M34 * right.M44, left.M41 * right.M11 + left.M42 * right.M21 + left.M43 * right.M31 + left.M44 * right.M41, left.M41 * right.M12 + left.M42 * right.M22 + left.M43 * right.M32 + left.M44 * right.M42, left.M41 * right.M13 + left.M42 * right.M23 + left.M43 * right.M33 + left.M44 * right.M43, left.M41 * right.M14 + left.M42 * right.M24 + left.M43 * right.M34 + left.M44 * right.M44);
        }
        
        [Obsolete ("Use CreatePerspectiveFieldOfView instead.")]
        public static Matrix4 Perspective (float fovy, float aspect, float near, float far)
        {
            float num = near * (float)Math.Tan ((double)(0.5f * fovy));
            float num2 = -num;
            float left = num2 * aspect;
            float right = num * aspect;
            return Matrix4.Frustum (left, right, num2, num, near, far);
        }
        
        public static Matrix4 Rotate (Quaternion q)
        {
            Vector3 axis;
            float angle;
            q.ToAxisAngle (out axis, out angle);
            return Matrix4.CreateFromAxisAngle (axis, angle);
        }
        
        [Obsolete ("Use CreateFromAxisAngle instead.")]
        public static Matrix4 Rotate (Vector3 axis, float angle)
        {
            float num = (float)Math.Cos ((double)(-(double)angle));
            float num2 = (float)Math.Sin ((double)(-(double)angle));
            float num3 = 1f - num;
            axis.Normalize ();
            Matrix4 result;
            result.Row0 = new Vector4 (num3 * axis.X * axis.X + num, num3 * axis.X * axis.Y - num2 * axis.Z, num3 * axis.X * axis.Z + num2 * axis.Y, 0f);
            result.Row1 = new Vector4 (num3 * axis.X * axis.Y + num2 * axis.Z, num3 * axis.Y * axis.Y + num, num3 * axis.Y * axis.Z - num2 * axis.X, 0f);
            result.Row2 = new Vector4 (num3 * axis.X * axis.Z - num2 * axis.Y, num3 * axis.Y * axis.Z + num2 * axis.X, num3 * axis.Z * axis.Z + num, 0f);
            result.Row3 = Vector4.UnitW;
            return result;
        }
        
        [Obsolete ("Use CreateRotationX instead.")]
        public static Matrix4 RotateX (float angle)
        {
            float num = (float)Math.Cos ((double)angle);
            float num2 = (float)Math.Sin ((double)angle);
            Matrix4 result;
            result.Row0 = Vector4.UnitX;
            result.Row1 = new Vector4 (0f, num, num2, 0f);
            result.Row2 = new Vector4 (0f, -num2, num, 0f);
            result.Row3 = Vector4.UnitW;
            return result;
        }
        
        [Obsolete ("Use CreateRotationY instead.")]
        public static Matrix4 RotateY (float angle)
        {
            float num = (float)Math.Cos ((double)angle);
            float num2 = (float)Math.Sin ((double)angle);
            Matrix4 result;
            result.Row0 = new Vector4 (num, 0f, -num2, 0f);
            result.Row1 = Vector4.UnitY;
            result.Row2 = new Vector4 (num2, 0f, num, 0f);
            result.Row3 = Vector4.UnitW;
            return result;
        }
        
        [Obsolete ("Use CreateRotationZ instead.")]
        public static Matrix4 RotateZ (float angle)
        {
            float num = (float)Math.Cos ((double)angle);
            float num2 = (float)Math.Sin ((double)angle);
            Matrix4 result;
            result.Row0 = new Vector4 (num, num2, 0f, 0f);
            result.Row1 = new Vector4 (-num2, num, 0f, 0f);
            result.Row2 = Vector4.UnitZ;
            result.Row3 = Vector4.UnitW;
            return result;
        }
        
        public static Matrix4 Scale (float scale)
        {
            return Matrix4.Scale (scale, scale, scale);
        }
        
        public static Matrix4 Scale (Vector3 scale)
        {
            return Matrix4.Scale (scale.X, scale.Y, scale.Z);
        }
        
        public static Matrix4 Scale (float x, float y, float z)
        {
            Matrix4 result;
            result.Row0 = Vector4.UnitX * x;
            result.Row1 = Vector4.UnitY * y;
            result.Row2 = Vector4.UnitZ * z;
            result.Row3 = Vector4.UnitW;
            return result;
        }
        
        [Obsolete ("Use CreateTranslation instead.")]
        public static Matrix4 Translation (float x, float y, float z)
        {
            Matrix4 identity = Matrix4.Identity;
            identity.Row3 = new Vector4 (x, y, z, 1f);
            return identity;
        }
        
        [Obsolete ("Use CreateTranslation instead.")]
        public static Matrix4 Translation (Vector3 trans)
        {
            return Matrix4.Translation (trans.X, trans.Y, trans.Z);
        }
        
        public static void Transpose (ref Matrix4 mat, out Matrix4 result)
        {
            result.Row0 = mat.Column0;
            result.Row1 = mat.Column1;
            result.Row2 = mat.Column2;
            result.Row3 = mat.Column3;
        }
        
        public static Matrix4 Transpose (Matrix4 mat)
        {
            return new Matrix4 (mat.Column0, mat.Column1, mat.Column2, mat.Column3);
        }
        
        //
        // Methods
        //
        
        public override bool Equals (object obj)
        {
            return obj is Matrix4 && this.Equals ((Matrix4)obj);
        }
        
        public bool Equals (Matrix4 other)
        {
            return this.Row0 == other.Row0 && this.Row1 == other.Row1 && this.Row2 == other.Row2 && this.Row3 == other.Row3;
        }
        
        public override int GetHashCode ()
        {
            return this.Row0.GetHashCode () ^ this.Row1.GetHashCode () ^ this.Row2.GetHashCode () ^ this.Row3.GetHashCode ();
        }
        
        public void Invert ()
        {
            this = Matrix4.Invert (this);
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
            this = Matrix4.Transpose (this);
        }
        
        //
        // Operators
        //
        
        public static bool operator == (Matrix4 left, Matrix4 right)
        {
            return left.Equals (right);
        }
        
        public static bool operator != (Matrix4 left, Matrix4 right)
        {
            return !left.Equals (right);
        }
        
        public static Matrix4 operator * (Matrix4 left, Matrix4 right)
        {
            return Matrix4.Mult (left, right);
        }
    }
}
