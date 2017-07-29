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


class ChannelBalance(object):
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
        'balance': 'str'
    }

    attribute_map = {
        'balance': 'balance'
    }

    def __init__(self, balance=None):
        """
        ChannelBalance - a model defined in Swagger
        """

        self._balance = None

        if balance is not None:
          self.balance = balance

    @property
    def balance(self):
        """
        Gets the balance of this ChannelBalance.

        :return: The balance of this ChannelBalance.
        :rtype: str
        """
        return self._balance

    @balance.setter
    def balance(self, balance):
        """
        Sets the balance of this ChannelBalance.

        :param balance: The balance of this ChannelBalance.
        :type: str
        """

        self._balance = balance

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
        if not isinstance(other, ChannelBalance):
            return False

        return self.__dict__ == other.__dict__

    def __ne__(self, other):
        """
        Returns true if both objects are not equal
        """
        return not self == other
