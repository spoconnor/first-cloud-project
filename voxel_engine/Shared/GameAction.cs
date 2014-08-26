﻿using System;
using System.Net.Sockets;
using OpenTK;
using System.Diagnostics;

namespace Sean.World
{
	internal enum ActionType : ushort
	{
		Error, //Reserve this to make debugging easier by letting unassigned 0 be an error
		Connect, //Don't move this - Connect checks the connecting client's version. If it moves between versions that check won't work.
		Disconnect,
		GetWorld,
		AddBlock,
		AddBlockMulti,
		AddCuboid,
		AddStructure,
		RemoveBlock,
		RemoveBlockMulti,
		PlayerMove,
		ServerMsg,
		ChatMsg,
		ServerCommand,
		PlayerInfo,
		PlayerOption,
		AddBlockItem,
		RemoveBlockItem,
		PickupBlockItem,
		ThrowException,
		AddProjectile,
		ServerSync,
		AddStaticItem
	}

	internal abstract class GameAction
	{
		protected GameAction()
		{
		}

		/*
        private Server.NetworkPlayer _connectedPlayer;
		/// <summary>Player the GameAction is being sent to. Only used by Servers.</summary>
		internal Server.NetworkPlayer ConnectedPlayer
		{
			get { return _connectedPlayer; }
			set
			{
				if (_connectedPlayer != null) throw new Exception("You cannot re-assign the ConnectedPlayer in order to reuse this object because it gets sent to a queue. Create a new object instead.");
				_connectedPlayer = value;
				if (Config.IsServer) TcpClient = _connectedPlayer.TcpClient;
			}
		}*/
		//internal bool IsAdmin { get { return _connectedPlayer.IsAdmin; } }
		public TcpClient TcpClient { get; protected set; }
		internal abstract ActionType ActionType { get; }
		public abstract override string ToString();
		internal int DataLength;

		#region Send
		private byte[] _byteQueue;
		private int _byteQueueIndex;
		private bool _isQueued;
		protected virtual void Queue()
		{
			if (TcpClient == null) throw new Exception("Server forgot to set TcpClient.");
			if (!TcpClient.Connected) return;

			_byteQueue = new byte[sizeof(ushort) + sizeof(int) + DataLength];
			Write(BitConverter.GetBytes((ushort)ActionType), sizeof(ushort));
			Write(BitConverter.GetBytes(DataLength), sizeof(int));
		}

		//internal bool Immediate;
		internal virtual void Send()
		{
			if (!_isQueued)
			{
				Queue();
				if (_byteQueueIndex != _byteQueue.Length) throw new Exception(string.Format("{0} DataLength {1} + {2} but queued {3}", ActionType, sizeof(ushort) + sizeof(int), DataLength, _byteQueueIndex));

				//if (!Immediate)
				{
					_isQueued = true;
// TODO
//					ConnectedPlayer.SendQueue.Enqueue(this);
					return;
				}
			}

			try
			{
				lock (TcpClient)
				{
					TcpClient.GetStream().Write(_byteQueue, 0, _byteQueue.Length);
				}
			}
			catch (Exception ex)
			{
                Debug.WriteLine("ConnectedPlayer, exception {0}", ex.Message);
			}
		}

		protected void Write(byte[] buffer, int count)
		{
			Buffer.BlockCopy(buffer, 0, _byteQueue, _byteQueueIndex, count);
			_byteQueueIndex += count;
		}

		protected void Write(ref Position position)
		{
			Write(position.ToByteArray(), Position.SIZE);
		}

		protected void Write(ref Coords coords)
		{
			Write(coords.ToByteArray(), Coords.SIZE);
		}

		protected void Write(ref Vector3 vector)
		{
			Write(BitConverter.GetBytes(vector.X), sizeof(float));
			Write(BitConverter.GetBytes(vector.Y), sizeof(float));
			Write(BitConverter.GetBytes(vector.Z), sizeof(float));
		}

		protected void Write(bool x)
		{
			Write(BitConverter.GetBytes(x), sizeof(bool));
		}

		protected void Write(int x)
		{
			Write(BitConverter.GetBytes(x), sizeof(int));
		}

		protected void Write(ushort x)
		{
			Write(BitConverter.GetBytes(x), sizeof(ushort));
		}

		protected void Write(short x)
		{
			Write(BitConverter.GetBytes(x), sizeof(short));
		}

		protected void Write(float x)
		{
			Write(BitConverter.GetBytes(x), sizeof(float));
		}
		#endregion

		#region Receive
		internal virtual void Receive()
		{
			if (TcpClient == null) throw new Exception("Server forgot to set TcpClient.");

			lock (TcpClient) //this will generally already be locked but not all actions override this method
			{
				DataLength = BitConverter.ToInt32(ReadStream(sizeof(int)), 0);
			}
		}

		/// <summary>Helper to ensure the requested amount is all read before we continue.</summary>
		protected byte[] ReadStream(int length)
		{
			var bytes = new byte[length];
			var bytesRead = 0;
			while (bytesRead < length)
			{
				bytesRead += TcpClient.GetStream().Read(bytes, bytesRead, length - bytesRead);
			}
			return bytes;
		}
		#endregion

	}
}
