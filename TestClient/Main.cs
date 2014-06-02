using System;
using RestSharp;

namespace TestClient
{
    class MainClass
    {
        static void Main()
        {
            var client = new RestClient("http://localhost:8888/WorldService");
            // client.Authenticator = new HttpBasicAuthenticator(username, password);
            
            var request = new RestRequest("hello/{msg}", Method.GET);
            //request.AddParameter("name", "value"); // adds to POST or URL querystring based on Method
            request.AddUrlSegment("msg", "Hi"); // replaces matching token in request.Resource
            
            // easily add HTTP Headers
            //request.AddHeader("header", "value");
            
            // add files to upload (works with compatible verbs)
            //request.AddFile(path);

            // execute the request
            var response = client.Execute(request);
            var content = response.Content; // raw content as string
            Console.WriteLine (content);

            /*
            // or automatically deserialize result
            // return content type is sniffed but can be explicitly set via RestClient.AddHandler();
            RestResponse<Person> response2 = client.Execute<Person>(request);
            var name = response2.Data.Name;
            
            // easy async support
            client.ExecuteAsync(request, response => {
                Console.WriteLine(response.Content);
            });
            
            // async with deserialization
            var asyncHandle = client.ExecuteAsync<Person>(request, response => {
                Console.WriteLine(response.Data.Name);
            });

            // abort the request on demand
            asyncHandle.Abort();
            */
        }
    }
}
