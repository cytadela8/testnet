from base import ApiUser, DEV_1_INT, DEV_2_INT, DEV_3_INT
from nose.tools import nottest

class GossipTest(ApiUser):
    def test_line_gossip(self):
        list_peers = [self.save_and_remove_peers(DEV_1_INT),
                      self.save_and_remove_peers(DEV_2_INT),
                      self.save_and_remove_peers(DEV_3_INT)]
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.1)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.3)
        self.assertEqual(self.top(DEV_1_INT, []), self.top(DEV_2_INT, []))
        self.assertEqual(self.top(DEV_1_INT, []), self.top(DEV_3_INT, []))
        self.add_peer(DEV_1_INT, [[127, 0, 0, 1], 3020])
        self.add_peer(DEV_2_INT, [[127, 0, 0, 1], 3030], sleep=0.1)
        for i in range(2):
            self.mine_block(DEV_1_INT, [1, 1], sleep=0.5)
            self.assertEqual(self.top(DEV_1_INT, []), self.top(DEV_2_INT, []))
            self.assertEqual(self.top(DEV_1_INT, []), self.top(DEV_3_INT, []))
        self.clear_and_add_peers(DEV_1_INT, list_peers[0])
        self.clear_and_add_peers(DEV_2_INT, list_peers[1])
        self.clear_and_add_peers(DEV_3_INT, list_peers[2])

    def test_tree_gossip(self):
        list_peers = [self.save_and_remove_peers(DEV_1_INT),
                      self.save_and_remove_peers(DEV_2_INT),
                      self.save_and_remove_peers(DEV_3_INT)]
        
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3020], sleep=0.1)
        self.sync(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.3)
        self.assertEqual(self.top(DEV_1_INT, []), self.top(DEV_2_INT, []))
        self.assertEqual(self.top(DEV_1_INT, []), self.top(DEV_3_INT, []))
        self.add_peer(DEV_1_INT, [[127, 0, 0, 1], 3020])
        self.add_peer(DEV_1_INT, [[127, 0, 0, 1], 3030], sleep=0.1)
        for i in range(2):
            self.mine_block(DEV_1_INT, [1, 1], sleep=0.5)
            self.assertEqual(self.top(DEV_1_INT, []), self.top(DEV_2_INT, []))
            self.assertEqual(self.top(DEV_1_INT, []), self.top(DEV_3_INT, []))

        self.clear_and_add_peers(DEV_1_INT, list_peers[0])
        self.clear_and_add_peers(DEV_2_INT, list_peers[1])
        self.clear_and_add_peers(DEV_3_INT, list_peers[2])

