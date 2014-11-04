### Generated by rprotoc. DO NOT EDIT!
### <proto file: CommsMessages.proto>
# package CommsMessages;
#  
# //import "google/protobuf/csharp_options.proto";
# //
# //option (google.protobuf.csharp_file_options).namespace = "Onewheel.Interface";
# //option (google.protobuf.csharp_file_options).umbrella_classname = "WorldEventsProtos";
# 
# option optimize_for = SPEED;
# 
# message RegisterClientRequest
# {
#   required string name = 1;
# }
# 
# message RegisterClientResponse
# {
#   required string motd = 1;
#   required int32 objectid = 2;
# }
# 
# message Message 
# {
#   required int32 from = 1;
#   required int32 target = 2;
#   required string message = 3;
# }
# 
# message Coords
# {
#   required int32 x = 1;
#   required int32 y = 2;
# }
# 
# message Movement
# {
#   required int32 object = 1;
#   required Coords from = 2;
#   required Coords to = 3;
#   required int32 speed = 4;
# }
# 
# message Action
# {
#   required int32 from = 1;
#   required Coords target = 2;
#   required string what = 3;
#   required string with = 4;
# }
# 
# message Object
# {
#   required Coords location = 1;
#   required int32 type = 2;
#   required ObjectAction action = 3;
#   required Coords destination = 4;
#   required int32 speed = 5;
# 
#   enum ObjectAction {
#     ADD = 0;
#     REMOVE = 1;
#     MOVE = 2;
#   }
# }
# 
# message Base {
#   enum MsgType {
#     RegisterClientRequest = 1;
#     RegisterClientResponse = 2;
#     Message = 3;
#     Movement = 4;
#     Action = 5;
#     Object = 6;
#   }
# 
#   // Identifies which field is filled in.
#   required MsgType msgtype = 1;
# 
#   // One of the following will be filled in.
#   optional RegisterClientRequest registerClientRequest = 2;
#   optional RegisterClientResponse registerClientResponse = 3;
#   optional Message message = 4;
#   optional Movement movement = 5;
#   optional Action action = 6;
#   optional Object object = 7;
# }
# 
# //service WebService {
# //  rpc RegisterClient (RegisterClientRequest) returns (Status);
# //  rpc Chat (Message) returns (Status);
# //  rpc DoAction (Action) returns (Status);
# //  rpc Move (Movement) returns (Status);
# //}
# 
# //message Person {
# //  required string name = 1;
# //  required int32 id = 2;
# //  optional string email = 3;
# //
# //  enum PhoneType {
# //    MOBILE = 0;
# //    HOME = 1;
# //    WORK = 2;
# //  }
# //
# //  message PhoneNumber {
# //    required string number = 1;
# //    optional PhoneType type = 2 [default = HOME];
# //  }
# //
# //  repeated PhoneNumber phone = 4;
# //}
# // 
# //// Our address book file is just one of these.
# //message AddressBook {
# //  repeated Person person = 1;
# //}
# 
# //message ServerInfoRequest {
# //  required string clientname = 1;
# //  optional int32 number = 2;
# //}
# //
# //message ServerInfoResponse {
# //  required string servername = 1;
# //}
# 

require 'protobuf/message/message'
require 'protobuf/message/enum'
require 'protobuf/message/service'
require 'protobuf/message/extend'

module CommsMessages
  ::Protobuf::OPTIONS[:"optimize_for"] = :SPEED
  class RegisterClientRequest < ::Protobuf::Message
    defined_in __FILE__
    required :string, :name, 1
  end
  class RegisterClientResponse < ::Protobuf::Message
    defined_in __FILE__
    required :string, :motd, 1
    required :int32, :objectid, 2
  end
  class Message < ::Protobuf::Message
    defined_in __FILE__
    required :int32, :from, 1
    required :int32, :target, 2
    required :string, :message, 3
  end
  class Coords < ::Protobuf::Message
    defined_in __FILE__
    required :int32, :x, 1
    required :int32, :y, 2
  end
  class Movement < ::Protobuf::Message
    defined_in __FILE__
    required :int32, :object, 1
    required :Coords, :from, 2
    required :Coords, :to, 3
    required :int32, :speed, 4
  end
  class Action < ::Protobuf::Message
    defined_in __FILE__
    required :int32, :from, 1
    required :Coords, :target, 2
    required :string, :what, 3
    required :string, :with, 4
  end
  class Object < ::Protobuf::Message
    defined_in __FILE__
    required :Coords, :location, 1
    required :int32, :type, 2
    required :ObjectAction, :action, 3
    required :Coords, :destination, 4
    required :int32, :speed, 5
    class ObjectAction < ::Protobuf::Enum
      defined_in __FILE__
      ADD = value(:ADD, 0)
      REMOVE = value(:REMOVE, 1)
      MOVE = value(:MOVE, 2)
    end
  end
  class Base < ::Protobuf::Message
    defined_in __FILE__
    class MsgType < ::Protobuf::Enum
      defined_in __FILE__
      RegisterClientRequest = value(:RegisterClientRequest, 1)
      RegisterClientResponse = value(:RegisterClientResponse, 2)
      Message = value(:Message, 3)
      Movement = value(:Movement, 4)
      Action = value(:Action, 5)
      Object = value(:Object, 6)
    end
    required :MsgType, :msgtype, 1
    optional :RegisterClientRequest, :registerClientRequest, 2
    optional :RegisterClientResponse, :registerClientResponse, 3
    optional :Message, :message, 4
    optional :Movement, :movement, 5
    optional :Action, :action, 6
    optional :Object, :object, 7
  end
end