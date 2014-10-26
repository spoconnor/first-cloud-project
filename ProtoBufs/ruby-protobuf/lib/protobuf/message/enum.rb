require 'delegate'
require 'protobuf/common/util'
require 'protobuf/descriptor/enum_descriptor'
require 'protobuf/message/protoable'

module Protobuf
  class Enum
    class <<self
      include Protoable

      attr_reader :values

      def name_by_value(value)
        if not defined?(@values)
          constants.find {|c| const_get(c) == value}  # for compatibility
        else
          @values_index ||= @values.inject({}) {|hash, (n, v)| hash[v.value.to_i] = n; hash }
          @values_index[value]
        end
      end

      alias get_name_by_tag name_by_value

      def valid_tag?(tag)
        !! name_by_value(tag)
      end

      def descriptor
        @descriptor ||= Descriptor::EnumDescriptor.new(self)
      end

      private

      # for compatibility
      def define(name, value)
        enum_value = EnumValue.new(self, name, value)
        const_set(Util.modulize(name), enum_value)
        (@values ||= {})[name] = enum_value
      end

      def value(name, val)
        enum_value = EnumValue.new(self, name, val)
        (@values ||= {})[name] = enum_value
        enum_value
      end
    end
  end

  class EnumValue < DelegateClass(Fixnum)

    attr_reader :parent_class, :name, :value

    def initialize(parent_class, name, value)
      @parent_class = parent_class
      @name = name
      @value = value
      super(@value)
    end

    def to_s
      @name.to_s
    end

    def inspect
      "\#<#{self.class} #{@parent_class}::#{@name}=#{@value}>"
    end
  end

end
