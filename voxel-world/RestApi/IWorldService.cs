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
            Method="GET",
            RequestFormat=WebMessageFormat.Json,
            ResponseFormat = WebMessageFormat.Json,
            BodyStyle=WebMessageBodyStyle.Wrapped, 
            UriTemplate = "chunk/{x}/{y}/{z}")]
        [OperationContract]
        string GetChunkAddress(int x, int y, int z);

        [WebInvoke(
            Method="GET",
            RequestFormat=WebMessageFormat.Json,
            ResponseFormat = WebMessageFormat.Json,
            BodyStyle=WebMessageBodyStyle.Wrapped, 
            UriTemplate = "showmapslice")]
            [OperationContract]
        ShowMapSliceResponse ShowMapSlice(ShowMapSliceRequest request);

        [WebInvoke(
            Method="GET",
            RequestFormat=WebMessageFormat.Json,
            ResponseFormat = WebMessageFormat.Json,
            BodyStyle=WebMessageBodyStyle.Wrapped, 
            UriTemplate = "setbloxk/{x}/{y}/{z}/{block}")]
            [OperationContract]
        bool SetBlock(int x, int y, int z, ushort block);
    }


    [DataContract,Serializable]
    public class ShowMapSliceRequest 
    {
      [DataMember]
      int x;
      [DataMember]
      int y;
      [DataMember]
      int z;
      [DataMember]
      int width;
      [DataMember]
      int height;
    }
    [DataContract,Serializable]
    public class ShowMapSliceResponse 
    {
      [DataMember]
      int width;
      [DataMember]
      int height;
      [DataMember]
      List<string> lines;
    }

}

