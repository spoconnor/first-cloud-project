using System;
using System.ServiceModel;
using System.ServiceModel.Web;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Json;
using System.Collections.Generic;

namespace Sean.World.RestApi
{
    [DataContract,Serializable]
    public struct Block
    {
        public Block(ushort data)
        {
            Data = data;
        }

        [DataMember]
        public ushort Data;
    }

    [DataContract,Serializable]
    public struct Position
    {
        public Position(int x, int y, int z)
        {
            X = x;
            Y = y;
            Z = z;
        }

        [DataMember]
        public int X;
        [DataMember]
        public int Y;
        [DataMember]
        public int Z;

        public override string ToString()
        {
            return string.Format("(x={0}, y={1}, z={2})", X, Y, Z);
        }
    }

    [ServiceContract(SessionMode=SessionMode.NotAllowed)] 
    public interface IWorldService
    {
        [WebInvoke(
            Method="GET",
            RequestFormat=WebMessageFormat.Json,
            ResponseFormat=WebMessageFormat.Json, 
            BodyStyle=WebMessageBodyStyle.Wrapped, 
            UriTemplate="hello/{msg}")] 
        [OperationContract]
        string HelloWorld(string msg);

        [WebInvoke(
            Method="POST",
            RequestFormat=WebMessageFormat.Json,
            ResponseFormat = WebMessageFormat.Json,
            BodyStyle=WebMessageBodyStyle.Wrapped, 
            UriTemplate = "chunk")]
        [OperationContract]
        string GetChunkAddress(int x, int y, int z);

        [WebInvoke(
            Method = "POST",
            RequestFormat=WebMessageFormat.Json,
            ResponseFormat = WebMessageFormat.Json,
            BodyStyle=WebMessageBodyStyle.Bare,
            //BodyStyle=WebMessageBodyStyle.Wrapped, 
            UriTemplate = "showchunk")]
        [OperationContract]
        ShowChunkResponse ShowChunk(ShowChunkRequest request);

        [WebInvoke(
            Method = "POST",
            RequestFormat=WebMessageFormat.Json,
            ResponseFormat = WebMessageFormat.Json,
            BodyStyle=WebMessageBodyStyle.Wrapped, 
            UriTemplate = "setblock")]
        [OperationContract]
        bool SetBlock(int x, int y, int z, ushort block);
    }


    [DataContract,Serializable]
    public class ShowChunkRequest 
    {
      [DataMember]
      public int h;
      [DataMember]
      public int x;
      [DataMember]
      public int y;
      [DataMember]
      public int z;
    }
    [DataContract,Serializable]
    public class ShowChunkResponse 
    {
      [DataMember]
      public int width;
      [DataMember]
      public int height;
      [DataMember]
      public List<string> lines;
    }

}

