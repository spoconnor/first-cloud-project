-- Generated By protoc-gen-lua Do not Edit
local protobuf = require "protobuf"
module('CommsMessages_pb')


local BASE = protobuf.Descriptor();
local BASE_PING = protobuf.Descriptor();
local BASE_REGISTER = protobuf.Descriptor();
local BASE_REGISTER_NAME_FIELD = protobuf.FieldDescriptor();
local BASE_REGISTERED = protobuf.Descriptor();
local BASE_REGISTERED_MOTD_FIELD = protobuf.FieldDescriptor();
local BASE_REGISTERED_OBJECTID_FIELD = protobuf.FieldDescriptor();
local BASE_SAY = protobuf.Descriptor();
local BASE_SAY_FROM_FIELD = protobuf.FieldDescriptor();
local BASE_SAY_TARGET_FIELD = protobuf.FieldDescriptor();
local BASE_SAY_TEXT_FIELD = protobuf.FieldDescriptor();
local BASE_COORDS = protobuf.Descriptor();
local BASE_COORDS_X_FIELD = protobuf.FieldDescriptor();
local BASE_COORDS_Y_FIELD = protobuf.FieldDescriptor();
local BASE_MOVE = protobuf.Descriptor();
local BASE_MOVE_OBJECT_FIELD = protobuf.FieldDescriptor();
local BASE_MOVE_FROM_FIELD = protobuf.FieldDescriptor();
local BASE_MOVE_TO_FIELD = protobuf.FieldDescriptor();
local BASE_MOVE_SPEED_FIELD = protobuf.FieldDescriptor();
local BASE_ACTION = protobuf.Descriptor();
local BASE_ACTION_FROM_FIELD = protobuf.FieldDescriptor();
local BASE_ACTION_TARGET_FIELD = protobuf.FieldDescriptor();
local BASE_ACTION_WHAT_FIELD = protobuf.FieldDescriptor();
local BASE_ACTION_WITH_FIELD = protobuf.FieldDescriptor();
local BASE_OBJECT = protobuf.Descriptor();
local BASE_OBJECT_OBJECTACTION = protobuf.EnumDescriptor();
local BASE_OBJECT_OBJECTACTION_ADD_ENUM = protobuf.EnumValueDescriptor();
local BASE_OBJECT_OBJECTACTION_REMOVE_ENUM = protobuf.EnumValueDescriptor();
local BASE_OBJECT_OBJECTACTION_MOVE_ENUM = protobuf.EnumValueDescriptor();
local BASE_OBJECT_LOCATION_FIELD = protobuf.FieldDescriptor();
local BASE_OBJECT_TYPE_FIELD = protobuf.FieldDescriptor();
local BASE_OBJECT_ACTION_FIELD = protobuf.FieldDescriptor();
local BASE_OBJECT_DESTINATION_FIELD = protobuf.FieldDescriptor();
local BASE_OBJECT_SPEED_FIELD = protobuf.FieldDescriptor();
local BASE_MSGTYPE = protobuf.EnumDescriptor();
local BASE_MSGTYPE_EPING_ENUM = protobuf.EnumValueDescriptor();
local BASE_MSGTYPE_EREGISTER_ENUM = protobuf.EnumValueDescriptor();
local BASE_MSGTYPE_EREGISTERED_ENUM = protobuf.EnumValueDescriptor();
local BASE_MSGTYPE_ESAY_ENUM = protobuf.EnumValueDescriptor();
local BASE_MSGTYPE_EMOVEMENT_ENUM = protobuf.EnumValueDescriptor();
local BASE_MSGTYPE_EACTION_ENUM = protobuf.EnumValueDescriptor();
local BASE_MSGTYPE_EOBJECT_ENUM = protobuf.EnumValueDescriptor();
local BASE_MSGTYPE_FIELD = protobuf.FieldDescriptor();
local BASE_PING_FIELD = protobuf.FieldDescriptor();
local BASE_REGISTER_FIELD = protobuf.FieldDescriptor();
local BASE_REGISTERED_FIELD = protobuf.FieldDescriptor();
local BASE_SAY_FIELD = protobuf.FieldDescriptor();
local BASE_MOVE_FIELD = protobuf.FieldDescriptor();
local BASE_ACTION_FIELD = protobuf.FieldDescriptor();
local BASE_OBJECT_FIELD = protobuf.FieldDescriptor();

BASE_PING.name = "Ping"
BASE_PING.full_name = ".CommsMessages.Base.Ping"
BASE_PING.nested_types = {}
BASE_PING.enum_types = {}
BASE_PING.fields = {}
BASE_PING.is_extendable = false
BASE_PING.extensions = {}
BASE_PING.containing_type = BASE
BASE_REGISTER_NAME_FIELD.name = "name"
BASE_REGISTER_NAME_FIELD.full_name = ".CommsMessages.Base.Register.name"
BASE_REGISTER_NAME_FIELD.number = 1
BASE_REGISTER_NAME_FIELD.index = 0
BASE_REGISTER_NAME_FIELD.label = 2
BASE_REGISTER_NAME_FIELD.has_default_value = false
BASE_REGISTER_NAME_FIELD.default_value = ""
BASE_REGISTER_NAME_FIELD.type = 9
BASE_REGISTER_NAME_FIELD.cpp_type = 9

BASE_REGISTER.name = "Register"
BASE_REGISTER.full_name = ".CommsMessages.Base.Register"
BASE_REGISTER.nested_types = {}
BASE_REGISTER.enum_types = {}
BASE_REGISTER.fields = {BASE_REGISTER_NAME_FIELD}
BASE_REGISTER.is_extendable = false
BASE_REGISTER.extensions = {}
BASE_REGISTER.containing_type = BASE
BASE_REGISTERED_MOTD_FIELD.name = "motd"
BASE_REGISTERED_MOTD_FIELD.full_name = ".CommsMessages.Base.Registered.motd"
BASE_REGISTERED_MOTD_FIELD.number = 1
BASE_REGISTERED_MOTD_FIELD.index = 0
BASE_REGISTERED_MOTD_FIELD.label = 2
BASE_REGISTERED_MOTD_FIELD.has_default_value = false
BASE_REGISTERED_MOTD_FIELD.default_value = ""
BASE_REGISTERED_MOTD_FIELD.type = 9
BASE_REGISTERED_MOTD_FIELD.cpp_type = 9

BASE_REGISTERED_OBJECTID_FIELD.name = "objectid"
BASE_REGISTERED_OBJECTID_FIELD.full_name = ".CommsMessages.Base.Registered.objectid"
BASE_REGISTERED_OBJECTID_FIELD.number = 2
BASE_REGISTERED_OBJECTID_FIELD.index = 1
BASE_REGISTERED_OBJECTID_FIELD.label = 2
BASE_REGISTERED_OBJECTID_FIELD.has_default_value = false
BASE_REGISTERED_OBJECTID_FIELD.default_value = 0
BASE_REGISTERED_OBJECTID_FIELD.type = 5
BASE_REGISTERED_OBJECTID_FIELD.cpp_type = 1

BASE_REGISTERED.name = "Registered"
BASE_REGISTERED.full_name = ".CommsMessages.Base.Registered"
BASE_REGISTERED.nested_types = {}
BASE_REGISTERED.enum_types = {}
BASE_REGISTERED.fields = {BASE_REGISTERED_MOTD_FIELD, BASE_REGISTERED_OBJECTID_FIELD}
BASE_REGISTERED.is_extendable = false
BASE_REGISTERED.extensions = {}
BASE_REGISTERED.containing_type = BASE
BASE_SAY_FROM_FIELD.name = "from"
BASE_SAY_FROM_FIELD.full_name = ".CommsMessages.Base.Say.from"
BASE_SAY_FROM_FIELD.number = 1
BASE_SAY_FROM_FIELD.index = 0
BASE_SAY_FROM_FIELD.label = 2
BASE_SAY_FROM_FIELD.has_default_value = false
BASE_SAY_FROM_FIELD.default_value = 0
BASE_SAY_FROM_FIELD.type = 5
BASE_SAY_FROM_FIELD.cpp_type = 1

BASE_SAY_TARGET_FIELD.name = "target"
BASE_SAY_TARGET_FIELD.full_name = ".CommsMessages.Base.Say.target"
BASE_SAY_TARGET_FIELD.number = 2
BASE_SAY_TARGET_FIELD.index = 1
BASE_SAY_TARGET_FIELD.label = 2
BASE_SAY_TARGET_FIELD.has_default_value = false
BASE_SAY_TARGET_FIELD.default_value = 0
BASE_SAY_TARGET_FIELD.type = 5
BASE_SAY_TARGET_FIELD.cpp_type = 1

BASE_SAY_TEXT_FIELD.name = "text"
BASE_SAY_TEXT_FIELD.full_name = ".CommsMessages.Base.Say.text"
BASE_SAY_TEXT_FIELD.number = 3
BASE_SAY_TEXT_FIELD.index = 2
BASE_SAY_TEXT_FIELD.label = 2
BASE_SAY_TEXT_FIELD.has_default_value = false
BASE_SAY_TEXT_FIELD.default_value = ""
BASE_SAY_TEXT_FIELD.type = 9
BASE_SAY_TEXT_FIELD.cpp_type = 9

BASE_SAY.name = "Say"
BASE_SAY.full_name = ".CommsMessages.Base.Say"
BASE_SAY.nested_types = {}
BASE_SAY.enum_types = {}
BASE_SAY.fields = {BASE_SAY_FROM_FIELD, BASE_SAY_TARGET_FIELD, BASE_SAY_TEXT_FIELD}
BASE_SAY.is_extendable = false
BASE_SAY.extensions = {}
BASE_SAY.containing_type = BASE
BASE_COORDS_X_FIELD.name = "x"
BASE_COORDS_X_FIELD.full_name = ".CommsMessages.Base.Coords.x"
BASE_COORDS_X_FIELD.number = 1
BASE_COORDS_X_FIELD.index = 0
BASE_COORDS_X_FIELD.label = 2
BASE_COORDS_X_FIELD.has_default_value = false
BASE_COORDS_X_FIELD.default_value = 0
BASE_COORDS_X_FIELD.type = 5
BASE_COORDS_X_FIELD.cpp_type = 1

BASE_COORDS_Y_FIELD.name = "y"
BASE_COORDS_Y_FIELD.full_name = ".CommsMessages.Base.Coords.y"
BASE_COORDS_Y_FIELD.number = 2
BASE_COORDS_Y_FIELD.index = 1
BASE_COORDS_Y_FIELD.label = 2
BASE_COORDS_Y_FIELD.has_default_value = false
BASE_COORDS_Y_FIELD.default_value = 0
BASE_COORDS_Y_FIELD.type = 5
BASE_COORDS_Y_FIELD.cpp_type = 1

BASE_COORDS.name = "Coords"
BASE_COORDS.full_name = ".CommsMessages.Base.Coords"
BASE_COORDS.nested_types = {}
BASE_COORDS.enum_types = {}
BASE_COORDS.fields = {BASE_COORDS_X_FIELD, BASE_COORDS_Y_FIELD}
BASE_COORDS.is_extendable = false
BASE_COORDS.extensions = {}
BASE_COORDS.containing_type = BASE
BASE_MOVE_OBJECT_FIELD.name = "object"
BASE_MOVE_OBJECT_FIELD.full_name = ".CommsMessages.Base.Move.object"
BASE_MOVE_OBJECT_FIELD.number = 1
BASE_MOVE_OBJECT_FIELD.index = 0
BASE_MOVE_OBJECT_FIELD.label = 2
BASE_MOVE_OBJECT_FIELD.has_default_value = false
BASE_MOVE_OBJECT_FIELD.default_value = 0
BASE_MOVE_OBJECT_FIELD.type = 5
BASE_MOVE_OBJECT_FIELD.cpp_type = 1

BASE_MOVE_FROM_FIELD.name = "from"
BASE_MOVE_FROM_FIELD.full_name = ".CommsMessages.Base.Move.from"
BASE_MOVE_FROM_FIELD.number = 2
BASE_MOVE_FROM_FIELD.index = 1
BASE_MOVE_FROM_FIELD.label = 2
BASE_MOVE_FROM_FIELD.has_default_value = false
BASE_MOVE_FROM_FIELD.default_value = nil
BASE_MOVE_FROM_FIELD.message_type = BASE_COORDS
BASE_MOVE_FROM_FIELD.type = 11
BASE_MOVE_FROM_FIELD.cpp_type = 10

BASE_MOVE_TO_FIELD.name = "to"
BASE_MOVE_TO_FIELD.full_name = ".CommsMessages.Base.Move.to"
BASE_MOVE_TO_FIELD.number = 3
BASE_MOVE_TO_FIELD.index = 2
BASE_MOVE_TO_FIELD.label = 2
BASE_MOVE_TO_FIELD.has_default_value = false
BASE_MOVE_TO_FIELD.default_value = nil
BASE_MOVE_TO_FIELD.message_type = BASE_COORDS
BASE_MOVE_TO_FIELD.type = 11
BASE_MOVE_TO_FIELD.cpp_type = 10

BASE_MOVE_SPEED_FIELD.name = "speed"
BASE_MOVE_SPEED_FIELD.full_name = ".CommsMessages.Base.Move.speed"
BASE_MOVE_SPEED_FIELD.number = 4
BASE_MOVE_SPEED_FIELD.index = 3
BASE_MOVE_SPEED_FIELD.label = 2
BASE_MOVE_SPEED_FIELD.has_default_value = false
BASE_MOVE_SPEED_FIELD.default_value = 0
BASE_MOVE_SPEED_FIELD.type = 5
BASE_MOVE_SPEED_FIELD.cpp_type = 1

BASE_MOVE.name = "Move"
BASE_MOVE.full_name = ".CommsMessages.Base.Move"
BASE_MOVE.nested_types = {}
BASE_MOVE.enum_types = {}
BASE_MOVE.fields = {BASE_MOVE_OBJECT_FIELD, BASE_MOVE_FROM_FIELD, BASE_MOVE_TO_FIELD, BASE_MOVE_SPEED_FIELD}
BASE_MOVE.is_extendable = false
BASE_MOVE.extensions = {}
BASE_MOVE.containing_type = BASE
BASE_ACTION_FROM_FIELD.name = "from"
BASE_ACTION_FROM_FIELD.full_name = ".CommsMessages.Base.Action.from"
BASE_ACTION_FROM_FIELD.number = 1
BASE_ACTION_FROM_FIELD.index = 0
BASE_ACTION_FROM_FIELD.label = 2
BASE_ACTION_FROM_FIELD.has_default_value = false
BASE_ACTION_FROM_FIELD.default_value = 0
BASE_ACTION_FROM_FIELD.type = 5
BASE_ACTION_FROM_FIELD.cpp_type = 1

BASE_ACTION_TARGET_FIELD.name = "target"
BASE_ACTION_TARGET_FIELD.full_name = ".CommsMessages.Base.Action.target"
BASE_ACTION_TARGET_FIELD.number = 2
BASE_ACTION_TARGET_FIELD.index = 1
BASE_ACTION_TARGET_FIELD.label = 2
BASE_ACTION_TARGET_FIELD.has_default_value = false
BASE_ACTION_TARGET_FIELD.default_value = nil
BASE_ACTION_TARGET_FIELD.message_type = BASE_COORDS
BASE_ACTION_TARGET_FIELD.type = 11
BASE_ACTION_TARGET_FIELD.cpp_type = 10

BASE_ACTION_WHAT_FIELD.name = "what"
BASE_ACTION_WHAT_FIELD.full_name = ".CommsMessages.Base.Action.what"
BASE_ACTION_WHAT_FIELD.number = 3
BASE_ACTION_WHAT_FIELD.index = 2
BASE_ACTION_WHAT_FIELD.label = 2
BASE_ACTION_WHAT_FIELD.has_default_value = false
BASE_ACTION_WHAT_FIELD.default_value = ""
BASE_ACTION_WHAT_FIELD.type = 9
BASE_ACTION_WHAT_FIELD.cpp_type = 9

BASE_ACTION_WITH_FIELD.name = "with"
BASE_ACTION_WITH_FIELD.full_name = ".CommsMessages.Base.Action.with"
BASE_ACTION_WITH_FIELD.number = 4
BASE_ACTION_WITH_FIELD.index = 3
BASE_ACTION_WITH_FIELD.label = 2
BASE_ACTION_WITH_FIELD.has_default_value = false
BASE_ACTION_WITH_FIELD.default_value = ""
BASE_ACTION_WITH_FIELD.type = 9
BASE_ACTION_WITH_FIELD.cpp_type = 9

BASE_ACTION.name = "Action"
BASE_ACTION.full_name = ".CommsMessages.Base.Action"
BASE_ACTION.nested_types = {}
BASE_ACTION.enum_types = {}
BASE_ACTION.fields = {BASE_ACTION_FROM_FIELD, BASE_ACTION_TARGET_FIELD, BASE_ACTION_WHAT_FIELD, BASE_ACTION_WITH_FIELD}
BASE_ACTION.is_extendable = false
BASE_ACTION.extensions = {}
BASE_ACTION.containing_type = BASE
BASE_OBJECT_OBJECTACTION_ADD_ENUM.name = "ADD"
BASE_OBJECT_OBJECTACTION_ADD_ENUM.index = 0
BASE_OBJECT_OBJECTACTION_ADD_ENUM.number = 0
BASE_OBJECT_OBJECTACTION_REMOVE_ENUM.name = "REMOVE"
BASE_OBJECT_OBJECTACTION_REMOVE_ENUM.index = 1
BASE_OBJECT_OBJECTACTION_REMOVE_ENUM.number = 1
BASE_OBJECT_OBJECTACTION_MOVE_ENUM.name = "MOVE"
BASE_OBJECT_OBJECTACTION_MOVE_ENUM.index = 2
BASE_OBJECT_OBJECTACTION_MOVE_ENUM.number = 2
BASE_OBJECT_OBJECTACTION.name = "ObjectAction"
BASE_OBJECT_OBJECTACTION.full_name = ".CommsMessages.Base.Object.ObjectAction"
BASE_OBJECT_OBJECTACTION.values = {BASE_OBJECT_OBJECTACTION_ADD_ENUM,BASE_OBJECT_OBJECTACTION_REMOVE_ENUM,BASE_OBJECT_OBJECTACTION_MOVE_ENUM}
BASE_OBJECT_LOCATION_FIELD.name = "location"
BASE_OBJECT_LOCATION_FIELD.full_name = ".CommsMessages.Base.Object.location"
BASE_OBJECT_LOCATION_FIELD.number = 1
BASE_OBJECT_LOCATION_FIELD.index = 0
BASE_OBJECT_LOCATION_FIELD.label = 2
BASE_OBJECT_LOCATION_FIELD.has_default_value = false
BASE_OBJECT_LOCATION_FIELD.default_value = nil
BASE_OBJECT_LOCATION_FIELD.message_type = BASE_COORDS
BASE_OBJECT_LOCATION_FIELD.type = 11
BASE_OBJECT_LOCATION_FIELD.cpp_type = 10

BASE_OBJECT_TYPE_FIELD.name = "type"
BASE_OBJECT_TYPE_FIELD.full_name = ".CommsMessages.Base.Object.type"
BASE_OBJECT_TYPE_FIELD.number = 2
BASE_OBJECT_TYPE_FIELD.index = 1
BASE_OBJECT_TYPE_FIELD.label = 2
BASE_OBJECT_TYPE_FIELD.has_default_value = false
BASE_OBJECT_TYPE_FIELD.default_value = 0
BASE_OBJECT_TYPE_FIELD.type = 5
BASE_OBJECT_TYPE_FIELD.cpp_type = 1

BASE_OBJECT_ACTION_FIELD.name = "action"
BASE_OBJECT_ACTION_FIELD.full_name = ".CommsMessages.Base.Object.action"
BASE_OBJECT_ACTION_FIELD.number = 3
BASE_OBJECT_ACTION_FIELD.index = 2
BASE_OBJECT_ACTION_FIELD.label = 2
BASE_OBJECT_ACTION_FIELD.has_default_value = false
BASE_OBJECT_ACTION_FIELD.default_value = nil
BASE_OBJECT_ACTION_FIELD.enum_type = BASE_OBJECT_OBJECTACTION
BASE_OBJECT_ACTION_FIELD.type = 14
BASE_OBJECT_ACTION_FIELD.cpp_type = 8

BASE_OBJECT_DESTINATION_FIELD.name = "destination"
BASE_OBJECT_DESTINATION_FIELD.full_name = ".CommsMessages.Base.Object.destination"
BASE_OBJECT_DESTINATION_FIELD.number = 4
BASE_OBJECT_DESTINATION_FIELD.index = 3
BASE_OBJECT_DESTINATION_FIELD.label = 2
BASE_OBJECT_DESTINATION_FIELD.has_default_value = false
BASE_OBJECT_DESTINATION_FIELD.default_value = nil
BASE_OBJECT_DESTINATION_FIELD.message_type = BASE_COORDS
BASE_OBJECT_DESTINATION_FIELD.type = 11
BASE_OBJECT_DESTINATION_FIELD.cpp_type = 10

BASE_OBJECT_SPEED_FIELD.name = "speed"
BASE_OBJECT_SPEED_FIELD.full_name = ".CommsMessages.Base.Object.speed"
BASE_OBJECT_SPEED_FIELD.number = 5
BASE_OBJECT_SPEED_FIELD.index = 4
BASE_OBJECT_SPEED_FIELD.label = 2
BASE_OBJECT_SPEED_FIELD.has_default_value = false
BASE_OBJECT_SPEED_FIELD.default_value = 0
BASE_OBJECT_SPEED_FIELD.type = 5
BASE_OBJECT_SPEED_FIELD.cpp_type = 1

BASE_OBJECT.name = "Object"
BASE_OBJECT.full_name = ".CommsMessages.Base.Object"
BASE_OBJECT.nested_types = {}
BASE_OBJECT.enum_types = {BASE_OBJECT_OBJECTACTION}
BASE_OBJECT.fields = {BASE_OBJECT_LOCATION_FIELD, BASE_OBJECT_TYPE_FIELD, BASE_OBJECT_ACTION_FIELD, BASE_OBJECT_DESTINATION_FIELD, BASE_OBJECT_SPEED_FIELD}
BASE_OBJECT.is_extendable = false
BASE_OBJECT.extensions = {}
BASE_OBJECT.containing_type = BASE
BASE_MSGTYPE_EPING_ENUM.name = "EPing"
BASE_MSGTYPE_EPING_ENUM.index = 0
BASE_MSGTYPE_EPING_ENUM.number = 1
BASE_MSGTYPE_EREGISTER_ENUM.name = "ERegister"
BASE_MSGTYPE_EREGISTER_ENUM.index = 1
BASE_MSGTYPE_EREGISTER_ENUM.number = 2
BASE_MSGTYPE_EREGISTERED_ENUM.name = "ERegistered"
BASE_MSGTYPE_EREGISTERED_ENUM.index = 2
BASE_MSGTYPE_EREGISTERED_ENUM.number = 3
BASE_MSGTYPE_ESAY_ENUM.name = "ESay"
BASE_MSGTYPE_ESAY_ENUM.index = 3
BASE_MSGTYPE_ESAY_ENUM.number = 4
BASE_MSGTYPE_EMOVEMENT_ENUM.name = "EMovement"
BASE_MSGTYPE_EMOVEMENT_ENUM.index = 4
BASE_MSGTYPE_EMOVEMENT_ENUM.number = 5
BASE_MSGTYPE_EACTION_ENUM.name = "EAction"
BASE_MSGTYPE_EACTION_ENUM.index = 5
BASE_MSGTYPE_EACTION_ENUM.number = 6
BASE_MSGTYPE_EOBJECT_ENUM.name = "EObject"
BASE_MSGTYPE_EOBJECT_ENUM.index = 6
BASE_MSGTYPE_EOBJECT_ENUM.number = 7
BASE_MSGTYPE.name = "MsgType"
BASE_MSGTYPE.full_name = ".CommsMessages.Base.MsgType"
BASE_MSGTYPE.values = {BASE_MSGTYPE_EPING_ENUM,BASE_MSGTYPE_EREGISTER_ENUM,BASE_MSGTYPE_EREGISTERED_ENUM,BASE_MSGTYPE_ESAY_ENUM,BASE_MSGTYPE_EMOVEMENT_ENUM,BASE_MSGTYPE_EACTION_ENUM,BASE_MSGTYPE_EOBJECT_ENUM}
BASE_MSGTYPE_FIELD.name = "msgtype"
BASE_MSGTYPE_FIELD.full_name = ".CommsMessages.Base.msgtype"
BASE_MSGTYPE_FIELD.number = 1
BASE_MSGTYPE_FIELD.index = 0
BASE_MSGTYPE_FIELD.label = 2
BASE_MSGTYPE_FIELD.has_default_value = false
BASE_MSGTYPE_FIELD.default_value = nil
BASE_MSGTYPE_FIELD.enum_type = BASE_MSGTYPE
BASE_MSGTYPE_FIELD.type = 14
BASE_MSGTYPE_FIELD.cpp_type = 8

BASE_PING_FIELD.name = "ping"
BASE_PING_FIELD.full_name = ".CommsMessages.Base.ping"
BASE_PING_FIELD.number = 2
BASE_PING_FIELD.index = 1
BASE_PING_FIELD.label = 1
BASE_PING_FIELD.has_default_value = false
BASE_PING_FIELD.default_value = nil
BASE_PING_FIELD.message_type = BASE_PING
BASE_PING_FIELD.type = 11
BASE_PING_FIELD.cpp_type = 10

BASE_REGISTER_FIELD.name = "register"
BASE_REGISTER_FIELD.full_name = ".CommsMessages.Base.register"
BASE_REGISTER_FIELD.number = 3
BASE_REGISTER_FIELD.index = 2
BASE_REGISTER_FIELD.label = 1
BASE_REGISTER_FIELD.has_default_value = false
BASE_REGISTER_FIELD.default_value = nil
BASE_REGISTER_FIELD.message_type = BASE_REGISTER
BASE_REGISTER_FIELD.type = 11
BASE_REGISTER_FIELD.cpp_type = 10

BASE_REGISTERED_FIELD.name = "registered"
BASE_REGISTERED_FIELD.full_name = ".CommsMessages.Base.registered"
BASE_REGISTERED_FIELD.number = 4
BASE_REGISTERED_FIELD.index = 3
BASE_REGISTERED_FIELD.label = 1
BASE_REGISTERED_FIELD.has_default_value = false
BASE_REGISTERED_FIELD.default_value = nil
BASE_REGISTERED_FIELD.message_type = BASE_REGISTERED
BASE_REGISTERED_FIELD.type = 11
BASE_REGISTERED_FIELD.cpp_type = 10

BASE_SAY_FIELD.name = "say"
BASE_SAY_FIELD.full_name = ".CommsMessages.Base.say"
BASE_SAY_FIELD.number = 5
BASE_SAY_FIELD.index = 4
BASE_SAY_FIELD.label = 1
BASE_SAY_FIELD.has_default_value = false
BASE_SAY_FIELD.default_value = nil
BASE_SAY_FIELD.message_type = BASE_SAY
BASE_SAY_FIELD.type = 11
BASE_SAY_FIELD.cpp_type = 10

BASE_MOVE_FIELD.name = "move"
BASE_MOVE_FIELD.full_name = ".CommsMessages.Base.move"
BASE_MOVE_FIELD.number = 6
BASE_MOVE_FIELD.index = 5
BASE_MOVE_FIELD.label = 1
BASE_MOVE_FIELD.has_default_value = false
BASE_MOVE_FIELD.default_value = nil
BASE_MOVE_FIELD.message_type = BASE_MOVE
BASE_MOVE_FIELD.type = 11
BASE_MOVE_FIELD.cpp_type = 10

BASE_ACTION_FIELD.name = "action"
BASE_ACTION_FIELD.full_name = ".CommsMessages.Base.action"
BASE_ACTION_FIELD.number = 7
BASE_ACTION_FIELD.index = 6
BASE_ACTION_FIELD.label = 1
BASE_ACTION_FIELD.has_default_value = false
BASE_ACTION_FIELD.default_value = nil
BASE_ACTION_FIELD.message_type = BASE_ACTION
BASE_ACTION_FIELD.type = 11
BASE_ACTION_FIELD.cpp_type = 10

BASE_OBJECT_FIELD.name = "object"
BASE_OBJECT_FIELD.full_name = ".CommsMessages.Base.object"
BASE_OBJECT_FIELD.number = 8
BASE_OBJECT_FIELD.index = 7
BASE_OBJECT_FIELD.label = 1
BASE_OBJECT_FIELD.has_default_value = false
BASE_OBJECT_FIELD.default_value = nil
BASE_OBJECT_FIELD.message_type = BASE_OBJECT
BASE_OBJECT_FIELD.type = 11
BASE_OBJECT_FIELD.cpp_type = 10

BASE.name = "Base"
BASE.full_name = ".CommsMessages.Base"
BASE.nested_types = {BASE_PING, BASE_REGISTER, BASE_REGISTERED, BASE_SAY, BASE_COORDS, BASE_MOVE, BASE_ACTION, BASE_OBJECT}
BASE.enum_types = {BASE_MSGTYPE}
BASE.fields = {BASE_MSGTYPE_FIELD, BASE_PING_FIELD, BASE_REGISTER_FIELD, BASE_REGISTERED_FIELD, BASE_SAY_FIELD, BASE_MOVE_FIELD, BASE_ACTION_FIELD, BASE_OBJECT_FIELD}
BASE.is_extendable = false
BASE.extensions = {}

Base = protobuf.Message(BASE)
Base.Action = protobuf.Message(BASE_ACTION)
Base.Coords = protobuf.Message(BASE_COORDS)
Base.Move = protobuf.Message(BASE_MOVE)
Base.Object = protobuf.Message(BASE_OBJECT)
Base.Ping = protobuf.Message(BASE_PING)
Base.Register = protobuf.Message(BASE_REGISTER)
Base.Registered = protobuf.Message(BASE_REGISTERED)
Base.Say = protobuf.Message(BASE_SAY)

