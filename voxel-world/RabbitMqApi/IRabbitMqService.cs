using System;
using System.ServiceModel;
using System.Runtime.Serialization;

namespace voxelworld
{
    [ServiceContract]
    public interface IRabbitMqService
    {
        [OperationContract(IsOneWay=true)]
        void Log(LogData entry);
    }

    [DataContract]
    public class LogData
    {
        private LogLevel m_level;
        private String m_message;
        private DateTime m_timeStamp;
        
        public LogData()
        {
            m_timeStamp = DateTime.Now;
        }
        
        public LogData(LogLevel level, String message)
            :base()
        {
            m_level = level;
            m_message = message;
        }
        
        [DataMember]
        public LogLevel Level
        {
            get { return m_level; }
            set { m_level = value; }
        }
        
        [DataMember]
        public String Message
        {
            get { return m_message; }
            set { m_message = value; }
        }
        
        [DataMember]
        public DateTime TimeStamp
        {
            get { return m_timeStamp; }
            set { m_timeStamp = value; }
        }
    }

    [DataContract]
    public enum LogLevel
    {
        [EnumMember]
        High,
        [EnumMember]
        Medium,
        [EnumMember]
        Low,
    }
}

