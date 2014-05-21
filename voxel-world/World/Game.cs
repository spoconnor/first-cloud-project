using System;
using System.Diagnostics;
using System.Linq;
using Sean;
using Sean.World;
using System.Collections.Generic;
using OpenTK;

namespace Sean
{
    internal class Game
    {
        #region Constructors
        //gm: from the OpenTK source code (Graphics\GraphicsMode.cs), here is GraphicsMode.Default, it does seem to select sensible choices -> default display bpp, 16 bit depth buffer, 0 bit stencil buffer, 2 buffers
        public Game ()
        {
            
            Width = Constants.DEFAULT_GAME_WIDTH;
            Height = Constants.DEFAULT_GAME_HEIGHT;
           

            //note: cant easily thread these loading tasks because they all need to run on the thread that creates the GL context
            Settings.Game = this;


            WorldHost = new WorldHost();
            
            CalculateProjectionMatrix ();
            UpdateFrustum ();
            WorldHost.BuildWorld ();
        }
#endregion
        
        #region Static Properties
        public static WorldHost WorldHost;
        public static Matrix4d Projection;
        public static Matrix4d ModelView;
        public static Vector4d LeftFrustum;
        public static Vector4d RightFrustum;
        public static Vector4d BottomFrustum;
        public static Vector4d TopFrustum;
        public static Vector4d NearFrustum;
        public static Vector4d FarFrustum;
        
        // Camera
        public static Coords PlayerCoords = new Coords();
        public static Coords CameraCoords = new Coords();
        public static float cameraRange = 40.0f;
        public static float cameraAngle = MathHelper.PiOver4;

        public static int Height;
        public static int Width;
        
        //public static ScriptDriver scriptDriver = new ScriptDriver ();
#endregion
        
        /// <summary>Calculate the projection matrix. Needs to be done on initial startup and anytime game width, height or FOV changes.</summary>
        internal void CalculateProjectionMatrix ()
        {
            Matrix4d.CreatePerspectiveFieldOfView (Settings.FieldOfView, Width / (float)Height, 0.01f, Settings.ZFar, out Projection);
        }

        /// <summary>Update the ModelView and Frustum. Done on every update cycle and before the world is initially loaded so we can preload chunks in the initial frustum.</summary>
        private static void UpdateFrustum ()
        {
            //ModelView = Matrix4d.LookAt (
            //    Player.Coords.Xf, Player.Coords.Yf + Constants.PLAYER_EYE_LEVEL, Player.Coords.Zf, 
            //    Player.Coords.Xf + (float)Math.Cos (Player.Coords.Direction) * (float)Math.Cos (Player.Coords.Pitch), 
            //    Player.Coords.Yf + Constants.PLAYER_EYE_LEVEL + (float)Math.Sin (Player.Coords.Pitch), 
            //    Player.Coords.Zf + (float)Math.Sin (Player.Coords.Direction) * (float)Math.Cos (Player.Coords.Pitch), 
            //    0, 1, 0);
            
            CameraCoords.Xf = PlayerCoords.Xf - (float)Math.Cos (PlayerCoords.Direction) * (float)Math.Sin (cameraAngle) * cameraRange;
            CameraCoords.Yf = PlayerCoords.Yf + (float)Math.Cos (cameraAngle) * cameraRange;
            CameraCoords.Zf = PlayerCoords.Zf - (float)Math.Sin (PlayerCoords.Direction) * (float)Math.Sin (cameraAngle) * cameraRange;
            ModelView = Matrix4d.LookAt (
                CameraCoords.Xf, 
                CameraCoords.Yf, 
                CameraCoords.Zf,
                PlayerCoords.Xf,
                PlayerCoords.Yf,
                PlayerCoords.Zf, 
                0, 1, 0);
            
            Matrix4d clip;
            Matrix4d.Mult (ref ModelView, ref Projection, out clip);
            LeftFrustum = new Vector4d (clip.M14 + clip.M11, clip.M24 + clip.M21, clip.M34 + clip.M31, clip.M44 + clip.M41);
            LeftFrustum.NormalizeFast ();
            RightFrustum = new Vector4d (clip.M14 - clip.M11, clip.M24 - clip.M21, clip.M34 - clip.M31, clip.M44 - clip.M41);
            RightFrustum.NormalizeFast ();
            
            BottomFrustum = new Vector4d (clip.M14 + clip.M12, clip.M24 + clip.M22, clip.M34 + clip.M32, clip.M44 + clip.M42);
            BottomFrustum.NormalizeFast ();
            TopFrustum = new Vector4d (clip.M14 - clip.M12, clip.M24 - clip.M22, clip.M34 - clip.M32, clip.M44 - clip.M42);
            TopFrustum.NormalizeFast ();
            
            NearFrustum = new Vector4d (clip.M14 + clip.M13, clip.M24 + clip.M23, clip.M34 + clip.M33, clip.M44 + clip.M43);
            NearFrustum.NormalizeFast ();
            FarFrustum = new Vector4d (clip.M14 - clip.M13, clip.M24 - clip.M23, clip.M34 - clip.M33, clip.M44 - clip.M43);
            FarFrustum.NormalizeFast ();
        }

    }
}