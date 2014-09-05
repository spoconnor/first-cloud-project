using System;
using System.ServiceModel;
using System.ServiceModel.Web;

namespace dotnet_server
{
    [DataContract]
    public class ListVisibleObjectsRequest
    {
        //[DataMember]
        //public int ChunkWidth {get; set;}
    }

    [DataContract]
    public class ListVisibleObjectsResponse
    {
    }

    [DataContract]
    public class ListPathsRequest
    {
    }

    [DataContract]
    public class ListPathsResponse
    {
    }

    [DataContract]
    public class DoActionRequest
    {
    }

    [DataContract]
    public class DoActionResponse
    {
    }

    [ServiceContract(Name = "ActorService")]
    public interface IActorService
    {
        [OperationContract]
        [WebGet(UriTemplate = "/VisibleObjects/{x}/{y}", ResponseFormat = WebMessageFormat.Json)]
        ListVisibleObjectsResponse ListVisibleObjects(ListVisibleObjectsRequest request);

        [OperationContract]
        [WebGet(UriTemplate = "/Paths/{x}/{y}", ResponseFormat = WebMessageFormat.Json)]
        ListPathsResponse ListPaths(ListPathsRequest request);

        [OperationContract]
        [WebInvoke(Method = "POST", UriTemplate = "/MoveTo/{x}/{y}", RequestFormat = WebMessageFormat.Json)]
        MoveToResponse MoveTo(MoveToRequest request);

        [OperationContract]
        [WebInvoke(Method = "POST", UriTemplate = "/DoAction/{x}/{y}", RequestFormat = WebMessageFormat.Json)]
        DoActionResponse DoAction(DoActionRequest request);
    }
}

