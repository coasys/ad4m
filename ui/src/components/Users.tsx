import { useContext, useEffect, useState } from "preact/compat";
import { Ad4minContext } from "../context/Ad4minContext";
import { cardStyle, listStyle } from "./styles";
import type { UserStatistics } from "@coasys/ad4m";

const Users = () => {
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [users, setUsers] = useState<UserStatistics[]>([]);
  const [loading, setLoading] = useState(true);

  const getUsers = async () => {
    try {
      setLoading(true);
      const userList = await client!.runtime.listUsers();
      setUsers(userList);
    } catch (error) {
      console.error("Failed to load users:", error);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    if (client) {
      getUsers();
    }
  }, [client]);

  const formatLastSeen = (lastSeen: string | null | undefined) => {
    if (!lastSeen) return "Never";

    const date = new Date(lastSeen);
    if (isNaN(date.getTime())) return "Invalid date";

    const now = new Date();
    const diffMs = now.getTime() - date.getTime();
    const diffMins = Math.floor(diffMs / 60000);
    const diffHours = Math.floor(diffMins / 60);
    const diffDays = Math.floor(diffHours / 24);

    if (diffMins < 1) return "Just now";
    if (diffMins < 60) return `${diffMins} minute${diffMins > 1 ? 's' : ''} ago`;
    if (diffHours < 24) return `${diffHours} hour${diffHours > 1 ? 's' : ''} ago`;
    if (diffDays < 7) return `${diffDays} day${diffDays > 1 ? 's' : ''} ago`;

    return date.toLocaleDateString();
  };

  const getStatusBadge = (lastSeen: string | null | undefined) => {
    if (!lastSeen) return <j-badge variant="gray">Inactive</j-badge>;

    const date = new Date(lastSeen);
    if (isNaN(date.getTime())) return <j-badge variant="gray">Invalid</j-badge>;

    const diffMs = Date.now() - date.getTime();
    const diffMins = Math.floor(diffMs / 60000);

    if (diffMins < 5) return <j-badge variant="success">Online</j-badge>;
    if (diffMins < 30) return <j-badge variant="warning">Away</j-badge>;
    return <j-badge variant="gray">Offline</j-badge>;
  };

  if (loading) {
    return (
      <j-box pt="1000" px="800">
        <j-flex gap="400" direction="column" a="center" j="center">
          <j-spinner size="lg"></j-spinner>
          <j-text color="ui-500">Loading users...</j-text>
        </j-flex>
      </j-box>
    );
  }

  return (
    <div>
      {users.length === 0 ? (
        <j-box pt="1000" px="800">
          <j-flex gap="400" direction="column" a="center" j="center">
            <j-icon color="ui-500" size="xl" name="users"></j-icon>
            <j-flex direction="column" gap="300" j="center" a="center">
              <j-text nomargin color="black" size="700" weight="800">
                No users yet
              </j-text>
              <j-text align="center" weight="300" size="500" color="ui-500">
                Users will appear here when multi-user mode is enabled and
                users have been created.
              </j-text>
            </j-flex>
          </j-flex>
        </j-box>
      ) : (
        <div>
          <j-box px="800" pt="600" pb="400">
            <j-text variant="heading" size="700" weight="800">
              Active Users
            </j-text>
            <j-text size="400" color="ui-500">
              {users.length} user{users.length !== 1 ? 's' : ''} registered
            </j-text>
          </j-box>
          <div style={{ ...listStyle }}>
            {users.map((user, index) => (
              <div key={`user-${index}`} style={{ ...cardStyle, width: "100%" }}>
                <j-flex gap="500" direction="column">
                  <j-flex gap="400" a="center">
                    <div>
                      <j-avatar size="lg" hash={user.email}></j-avatar>
                    </div>
                    <j-flex direction="column" gap="100">
                      <j-flex gap="300" a="center">
                        <j-text nomargin variant="heading-sm" size="600" weight="600">
                          {user.email}
                        </j-text>
                        {getStatusBadge(user.lastSeen)}
                      </j-flex>
                      <j-text nomargin size="300" color="ui-500" style={{ wordBreak: 'break-all', fontFamily: 'monospace' }}>
                        {user.did || 'DID not set'}
                      </j-text>
                    </j-flex>
                  </j-flex>

                  <j-flex gap="600">
                    <j-flex direction="column" gap="100">
                      <j-text nomargin size="300" color="ui-500" weight="500">
                        LAST SEEN
                      </j-text>
                      <j-text nomargin size="400">
                        {formatLastSeen(user.lastSeen)}
                      </j-text>
                    </j-flex>

                    <j-flex direction="column" gap="100">
                      <j-text nomargin size="300" color="ui-500" weight="500">
                        PERSPECTIVES
                      </j-text>
                      <j-text nomargin size="400">
                        {user.perspectiveCount}
                      </j-text>
                    </j-flex>
                  </j-flex>
                </j-flex>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  );
};

export default Users;
