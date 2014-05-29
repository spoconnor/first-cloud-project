using System;
using System.ServiceModel;

namespace voxelworld
{
    [ServiceBehavior(InstanceContextMode=InstanceContextMode.Single)]
    public class RabbitMqService : IRabbitMqService
    {
        public int m_i;
        public void Log(LogData entry)
        {
            Console.WriteLine("RabbitMq:  [SVC] {3} [{0,-6}] {1, 12}: {2}", entry.Level, entry.TimeStamp, entry.Message, m_i++);
        }
    }

}

