using System;

namespace dotnet_server
{
    public class MapService : IMapService
    {
        public MapService ()
        {
        }

        #region IMapService implementation

        public void InitializeMap (InitializeMapRequest initializeMapRequest)
        {

        }

        public ListMapChunksResponse ListMapChunks ()
        {
            return new ListMapChunksResponse();
        }

        public GetMapChunkResponse GetMapChunk (int id)
        {
            return new GetMapChunkResponse();
        }

        public void GenerateMapChunk (int x, int y)
        {

        }

        #endregion
    }
}

