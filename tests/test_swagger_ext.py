import unittest
from swagger_client import ApiClient, InternalApi, ExternalApi
from swagger_client.rest import ApiException

class ExternalAPITest(unittest.TestCase):
    INT_API = {
        'dev1': InternalApi(ApiClient('http://localhost:3012/v1')),
        'dev2': InternalApi(ApiClient('http://localhost:3022/v1')),
        'dev3': InternalApi(ApiClient('http://localhost:3032/v1')),
    }
    API = {
        'dev1': ExternalApi(ApiClient('http://localhost:3013/v1')),
        'dev2': ExternalApi(ApiClient('http://localhost:3023/v1')),
        'dev3': ExternalApi(ApiClient('http://localhost:3033/v1')),
    }
    
    def test_header(self):
        api = self.API['dev1']
        header = api.get_header({'block-id': 0}) 
        self.assertEqual(header.block_id, 0)
        self.assertIsNotNone(header.block_header)

    def test_headers(self):
        int_api = self.INT_API['dev1']
        int_api.mine_block({'count': 1, 'times': 1})
        api = self.API['dev1']
        a = api.get_headers([{'block-id': 0}, {'block-id': 1}])
        self.assertEqual(len(a), 2)
