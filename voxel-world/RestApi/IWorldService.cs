using System;
using System.ServiceModel;
using System.ServiceModel.Web;

namespace voxelworld
{
    [ServiceContract]
    public interface IWorldService
    {
        [OperationContract]
        [WebInvoke(
            Method="GET", 
            ResponseFormat=WebMessageFormat.Json, 
            BodyStyle=WebMessageBodyStyle.Wrapped, 
            UriTemplate="json/{id}")]
        string HelloWorld(string id);
    }
}

