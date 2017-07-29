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


class Header(object):
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
        'block_id': 'int',
        'block_header': 'str'
    }

    attribute_map = {
        'block_id': 'block-id',
        'block_header': 'block-header'
    }

    def __init__(self, block_id=None, block_header=None):
        """
        Header - a model defined in Swagger
        """

        self._block_id = None
        self._block_header = None

        if block_id is not None:
          self.block_id = block_id
        if block_header is not None:
          self.block_header = block_header

    @property
    def block_id(self):
        """
        Gets the block_id of this Header.

        :return: The block_id of this Header.
        :rtype: int
        """
        return self._block_id

    @block_id.setter
    def block_id(self, block_id):
        """
        Sets the block_id of this Header.

        :param block_id: The block_id of this Header.
        :type: int
        """

        self._block_id = block_id

    @property
    def block_header(self):
        """
        Gets the block_header of this Header.

        :return: The block_header of this Header.
        :rtype: str
        """
        return self._block_header

    @block_header.setter
    def block_header(self, block_header):
        """
        Sets the block_header of this Header.

        :param block_header: The block_header of this Header.
        :type: str
        """

        self._block_header = block_header

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
        if not isinstance(other, Header):
            return False

        return self.__dict__ == other.__dict__

    def __ne__(self, other):
        """
        Returns true if both objects are not equal
        """
        return not self == other
