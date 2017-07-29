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


class MineBlock(object):
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
        'count': 'int',
        'times': 'int'
    }

    attribute_map = {
        'count': 'count',
        'times': 'times'
    }

    def __init__(self, count=None, times=None):
        """
        MineBlock - a model defined in Swagger
        """

        self._count = None
        self._times = None

        if count is not None:
          self.count = count
        if times is not None:
          self.times = times

    @property
    def count(self):
        """
        Gets the count of this MineBlock.

        :return: The count of this MineBlock.
        :rtype: int
        """
        return self._count

    @count.setter
    def count(self, count):
        """
        Sets the count of this MineBlock.

        :param count: The count of this MineBlock.
        :type: int
        """

        self._count = count

    @property
    def times(self):
        """
        Gets the times of this MineBlock.

        :return: The times of this MineBlock.
        :rtype: int
        """
        return self._times

    @times.setter
    def times(self, times):
        """
        Sets the times of this MineBlock.

        :param times: The times of this MineBlock.
        :type: int
        """

        self._times = times

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
        if not isinstance(other, MineBlock):
            return False

        return self.__dict__ == other.__dict__

    def __ne__(self, other):
        """
        Returns true if both objects are not equal
        """
        return not self == other
