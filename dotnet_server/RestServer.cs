using System;
using System.ServiceModel;
using System.ServiceModel.Web;
using System.ServiceModel.Activation;

namespace dotnet_server
{
    public static class Routing
    {
        public const string GetClientRoute = "/Client/{id}";
    }

    [ServiceContract(Name = "RESTDemoServices")]
    public interface IRESTDemoServices
    {
        [OperationContract]
        [WebGet(UriTemplate = Routing.GetClientRoute, BodyStyle = WebMessageBodyStyle.Bare)]
        string GetClientNameById(string Id);
    }

    [ServiceBehavior(InstanceContextMode = InstanceContextMode.Single, 
        ConcurrencyMode = ConcurrencyMode.Single, IncludeExceptionDetailInFaults = true)]
    [AspNetCompatibilityRequirements(RequirementsMode = AspNetCompatibilityRequirementsMode.Allowed)]
    public class RestDemoServices:IRESTDemoServices
    {
        public string GetClientNameById(string id)
        {
            Random r = new Random();
            string ReturnString="";
            int Idnum=Convert.ToInt32(id);
            for (int i = 0; i < Idnum; i++)
                ReturnString += char.ConvertFromUtf32(r.Next(65, 85));
            
            return ReturnString;
        }
    }

    public class RestServer
    {
        public RestServer ()
        {
        }

        private WebServiceHost _serviceHost;

        public void Start()
        {
            RestDemoServices DemoServices = new RestDemoServices();
            _serviceHost = new WebServiceHost(DemoServices, 
                 new Uri("http://localhost:8000/DEMOService"));
            _serviceHost.Open();
        }

        public void Stop()
        {
            _serviceHost.Close();
        }
    }
}

