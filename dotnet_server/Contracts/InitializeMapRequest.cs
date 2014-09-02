using System;
using System.Runtime.Serialization;

namespace dotnet_server
{
    [DataContract]
    public class InitializeMapRequest
    {
        [DataMember]
        public int ChunkWidth {get; set;}

        [DataMember]
        public int ChunkHeight {get; set;}
    }
}

