using System;

namespace Sean.World
{
    public class VoxelEngine
    {
        private VoxelEngine ()
        {
        }

        public static VoxelEngine Instance
        {
            get { return _instance; }
        }
        private static VoxelEngine _instance;

        public void Start()
        {

        }

    }
}

