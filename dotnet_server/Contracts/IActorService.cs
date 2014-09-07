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

    
    [DataContract]
    public class GetEventsResponse
    {
    }
    
    [DataContract]
    public class GetEventsRequest
    {
    }

    [ServiceContract(Name = "ActorService")]
    public interface IActorService
    {
        [OperationContract]
        [WebGet(UriTemplate = "/VisibleObjects", ResponseFormat = WebMessageFormat.Json)]
        ListVisibleObjectsResponse ListVisibleObjects(ListVisibleObjectsRequest request);

//        [OperationContract]
//        [WebGet(UriTemplate = "/Paths/{x}/{y}", ResponseFormat = WebMessageFormat.Json)]
//        ListPathsResponse ListPaths(ListPathsRequest request);

        [OperationContract]
        [WebInvoke(Method = "POST", UriTemplate = "/MoveTo", RequestFormat = WebMessageFormat.Json)]
        MoveToResponse MoveTo(MoveToRequest request);

        [OperationContract]
        [WebInvoke(Method = "POST", UriTemplate = "/DoAction", RequestFormat = WebMessageFormat.Json)]
        DoActionResponse DoAction(DoActionRequest request);

        [OperationContract]
        [WebGet(UriTemplate = "/Events", ResponseFormat = WebMessageFormat.Json)]
        GetEventsResponse GetEvents(GetEventsRequest request);

    }
}

