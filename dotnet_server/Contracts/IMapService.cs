using System;
using System.ServiceModel;
using System.ServiceModel.Web;

namespace dotnet_server
{
    [ServiceContract(Name = "MapService")]
    public interface IMapService
    {
        [OperationContract]
        [WebInvoke(Method = "POST", UriTemplate = "/Initialize", RequestFormat = WebMessageFormat.Json)]
        void InitializeMap(InitializeMapRequest initializeMapRequest);

        [OperationContract]
        // WebGet => Method = "POST",
        [WebGet(UriTemplate = "/Chunks", ResponseFormat = WebMessageFormat.Json)]
        ListMapChunksResponse ListMapChunks();

        [OperationContract]
        [WebGet(UriTemplate = "/Chunks/{x}/{y}", ResponseFormat = WebMessageFormat.Json)]
        GetMapChunkResponse GetMapChunk(int id);

        [OperationContract]
        [WebInvoke(Method = "POST", UriTemplate = "/Chunks/{x}/{y}", RequestFormat = WebMessageFormat.Json)]
        void GenerateMapChunk(int x, int y);
    }
}

