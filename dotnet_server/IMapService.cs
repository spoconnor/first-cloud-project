using System;

namespace dotnet_server
{
    [ServiceContract(Name = "MapService")]
    public interface IMapService
    {
        [OperationContract]
        [WebInvoke(Method = "POST", UriTemplate = "/Initialize", RequestFormat = WebMessageFormat.Json)]
        string InitializeMap();

        [OperationContract]
        // WebGet => Method = "POST",
        [WebGet(UriTemplate = "/Chunks", ResponseFormat = WebMessageFormat.Json)]
        string ListMapChunks();

        [OperationContract]
        [WebGet(UriTemplate = "/Chunks/{id}", ResponseFormat = WebMessageFormat.Json)]
        string GetMapChunk(int id);

        [OperationContract]
        [WebInvoke(Method = "POST", UriTemplate = "/Chunks/{x}/{y}", RequestFormat = WebMessageFormat.Json)]
        string GenerateMapChunk(int x, int y);
    }
}

