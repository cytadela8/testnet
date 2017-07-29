from time import sleep
import unittest
from nose.tools import nottest
from swagger_client import ApiClient, InternalApi
from swagger_client.rest import ApiException

class InternalAPITest(unittest.TestCase):
    CLIENT = {
        'dev1': ApiClient('http://localhost:3012/v1'),
        'dev2': ApiClient('http://localhost:3022/v1'),
        'dev3': ApiClient('http://localhost:3032/v1'),
    }

    API = {
        'dev1': InternalApi(CLIENT['dev1']),
        'dev2': InternalApi(CLIENT['dev2']),
        'dev3': InternalApi(CLIENT['dev3']),
    }
    
    def setUp(self):
        api = self.API['dev1']
        # save master key pair
        self.keypair = api.fetch_key_pair()
        self.assertIsNotNone(self.keypair.public)
        self.assertIsNotNone(self.keypair.private)

    def tearDown(self):
        api = self.API['dev1']
        # restore master key pair
        keypair = self.keypair.to_dict()
        keypair['brain-wallet'] = ''
        api.set_key_pair(keypair)

    @nottest
    def create_account(self, api, amount):
        keypair = api.create_key_pair()
        api.add_account({'pubkey': keypair.public, 'amount': amount})
        return keypair

    def test_create_keypair(self):
        api = self.API['dev1']
        keypair = api.create_key_pair()
        self.assertIsNotNone(keypair.public)
        self.assertIsNotNone(keypair.private)

    def test_create_account(self):
        api = self.API['dev1']
        keypair = self.create_account(api, 10)
        self.assertIsNotNone(keypair.public)
        self.assertIsNotNone(keypair.private)

    def test_top(self):
        api = self.API['dev1']
        top = api.get_top()
        self.assertIsNotNone(top.hash)
        self.assertIsNotNone(top.height)

    def test_add_peer(self):
        api = self.API['dev1']
        api.add_peer({'ip': '46.101.103.165', 'port': 8080})
        with self.assertRaises(ApiException) as cm:
            api.add_peer({'ip': '46.101.103165', 'port': 8080})
        self.assertEqual(cm.exception.status, 405)

    def test_spend(self):
        api = self.API['dev1']
        keypair = self.create_account(api, 10)
        api.spend({'pubkey': keypair.public, 'amount': 5})

    def test_sync(self):
        api = self.API['dev1']
        api.sync({'ip': '127.0.0.1', 'port': 3020})
        with self.assertRaises(ApiException) as cm:
            api.sync({'ip': '127.0.0.1.1', 'port': 3020})
        self.assertEqual(cm.exception.status, 405)


    def test_mine_block(self):
        api = self.API['dev1']
        api.mine_block({'count': 2, 'times': 1})

    def test_delete_account(self):
        api = self.API['dev1']
        master = api.fetch_pub_key().pubkey
        kp1 = self.create_account(api, 20)
        api.set_key_pair({'public': kp1.public, 'private': kp1.private, 'brain-wallet': ''}); sleep(1)
        kp2 = self.create_account(api, 10); sleep(0.5)
        api.delete_account({'pubkey': kp2.public}); sleep(0.5)
        api.fetch_account({'pubkey': master})
        with self.assertRaises(ApiException) as cm:
            api.fetch_account({'pubkey': kp1.public})
        self.assertEqual(cm.exception.status, 404)
        api.fetch_account({'pubkey': kp2.public})
        # XXX check pub2's balance?

    # todo: fix api:integer_channel_balance/0
    @nottest
    def test_channel_balance(self):
        api = self.API['dev1']
        balance = self.c(api.channel_balance())
        self.assertIsNotNone(balance)

    @nottest
    def test_channel_solo_close(self):
        api = self.API['dev1']
        self.c(api.channel_solo_close())

    @nottest
    def test_channel_timeout(self):
        api = self.API['dev1']
        self.c(api.channel_timeout())

    @nottest
    def test_add_secret(self):
        api = self.API['dev1']
        code = 'AgAAAAwr/nWTT4zbCS4lAuc='
        secret = 'WgAAAAAAOkYAAAAAMgAAAAABAAAAAACEC0dIFBQoAgAAAAx3wv4k7MKMmFva1BoKOhYUFhRGAAAAAAAAAAAAAgAAACcQRwAAAAAxAAAAAAEAAAAAAEiECw=='

        self.c(api.add_secret(code, secret))

    @nottest
    def test_lightning_payments(self):
        api1 = self.API['dev1']
        api2 = self.API['dev2']
        api3 = self.API['dev3']
        # grab dev1's pubkey
        pub1 = self.c(api1.fetch_pubkey())
        # sync dev1 with dev2 and dev3
        self.c(api1.sync('127.0.0.1', 3020)); sleep(0.5)
        self.c(api1.sync('127.0.0.1', 3030)); sleep(0.5)
        # set keys on dev2 and dev3
        pub2, priv2 = self.c(api2.create_keypair())
        self.c(api2.set_keypair(pub2, priv2))
        pub3, priv3 = self.c(api3.create_keypair())
        self.c(api3.set_keypair(pub3, priv3))
        # let dev1 know about dev2 and dev3
        self.c(api1.create_account_with_key(pub2, 10)); sleep(0.5)
        self.c(api1.create_account_with_key(pub3, 10)); sleep(0.5)
        # sync dev1 and dev2 with dev3
        self.c(api1.sync('127.0.0.1', 3030)); sleep(0.1)
        self.c(api2.sync('127.0.0.1', 3030)); sleep(0.1)
        # 2 step handshake to make channel
        self.c(api1.new_channel_with_server('127.0.0.1', 3030, 1, 10000, 10001, 50, 4)); sleep(0.1)
        self.c(api2.sync('127.0.0.1', 3030)); sleep(0.1)
        self.c(api2.new_channel_with_server('127.0.0.1', 3030, 2, 10000, 10001, 50, 4)); sleep(0.1)
        self.c(api1.sync('127.0.0.1', 3030)); sleep(0.1)
        # spend
        self.c(api1.channel_spend('127.0.0.1', 3030, 777)); sleep(0.1)
        self.c(api1.lightning_spend('127.0.0.1', 3030, pub2, 4, 10)); sleep(0.1)
        # XXX Can't pull dev1 first. Why?
        self.c(api2.pull_channel_state('127.0.0.1', 3030)); sleep(0.1)
        self.c(api1.pull_channel_state('127.0.0.1', 3030)); sleep(0.1)
        # spend again
        self.c(api2.lightning_spend('127.0.0.1', 3030, pub1, 4, 10)); sleep(0.1)
        self.c(api1.pull_channel_state('127.0.0.1', 3030)); sleep(0.1)
        self.c(api2.pull_channel_state('127.0.0.1', 3030)); sleep(0.1)

