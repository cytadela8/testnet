# coding: utf-8

"""
    Aeternity Testnet

    This is the [Aeternity](https://www.aeternity.com/) Testnet API.

    OpenAPI spec version: 1.0.0
    Contact: apiteam@aeternity.com
    Generated by: https://github.com/swagger-api/swagger-codegen.git
"""


from pprint import pformat
from six import iteritems
import re


class ChannelSpend(object):
    """
    NOTE: This class is auto generated by the swagger code generator program.
    Do not edit the class manually.
    """


    """
    Attributes:
      swagger_types (dict): The key is attribute name
                            and the value is attribute type.
      attribute_map (dict): The key is attribute name
                            and the value is json key in definition.
    """
    swagger_types = {
        'ip': 'str',
        'port': 'int',
        'amount': 'int'
    }

    attribute_map = {
        'ip': 'ip',
        'port': 'port',
        'amount': 'amount'
    }

    def __init__(self, ip=None, port=None, amount=None):
        """
        ChannelSpend - a model defined in Swagger
        """

        self._ip = None
        self._port = None
        self._amount = None

        if ip is not None:
          self.ip = ip
        if port is not None:
          self.port = port
        if amount is not None:
          self.amount = amount

    @property
    def ip(self):
        """
        Gets the ip of this ChannelSpend.

        :return: The ip of this ChannelSpend.
        :rtype: str
        """
        return self._ip

    @ip.setter
    def ip(self, ip):
        """
        Sets the ip of this ChannelSpend.

        :param ip: The ip of this ChannelSpend.
        :type: str
        """

        self._ip = ip

    @property
    def port(self):
        """
        Gets the port of this ChannelSpend.

        :return: The port of this ChannelSpend.
        :rtype: int
        """
        return self._port

    @port.setter
    def port(self, port):
        """
        Sets the port of this ChannelSpend.

        :param port: The port of this ChannelSpend.
        :type: int
        """

        self._port = port

    @property
    def amount(self):
        """
        Gets the amount of this ChannelSpend.

        :return: The amount of this ChannelSpend.
        :rtype: int
        """
        return self._amount

    @amount.setter
    def amount(self, amount):
        """
        Sets the amount of this ChannelSpend.

        :param amount: The amount of this ChannelSpend.
        :type: int
        """

        self._amount = amount

    def to_dict(self):
        """
        Returns the model properties as a dict
        """
        result = {}

        for attr, _ in iteritems(self.swagger_types):
            value = getattr(self, attr)
            if isinstance(value, list):
                result[attr] = list(map(
                    lambda x: x.to_dict() if hasattr(x, "to_dict") else x,
                    value
                ))
            elif hasattr(value, "to_dict"):
                result[attr] = value.to_dict()
            elif isinstance(value, dict):
                result[attr] = dict(map(
                    lambda item: (item[0], item[1].to_dict())
                    if hasattr(item[1], "to_dict") else item,
                    value.items()
                ))
            else:
                result[attr] = value

        return result

    def to_str(self):
        """
        Returns the string representation of the model
        """
        return pformat(self.to_dict())

    def __repr__(self):
        """
        For `print` and `pprint`
        """
        return self.to_str()

    def __eq__(self, other):
        """
        Returns true if both objects are equal
        """
        if not isinstance(other, ChannelSpend):
            return False

        return self.__dict__ == other.__dict__

    def __ne__(self, other):
        """
        Returns true if both objects are not equal
        """
        return not self == other
