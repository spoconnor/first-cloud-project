using System;

namespace dotnet_server
{
    class MainClass
    {
        public static void Main (string[] args)
        {
            Console.WriteLine ("Hello World!");
            var restServer = new RestServer();
            restServer.Start();

            Console.ReadKey();
            restServer.Stop();
        }
    }
}
