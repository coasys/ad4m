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
  const [creditAmounts, setCreditAmounts] = useState<Record<string, string>>({});
  const [actionLoading, setActionLoading] = useState<Record<string, boolean>>({});

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

  const handleToggleFreeAccess = async (email: string, currentFreeAccess: boolean) => {
    const key = `free-${email}`;
    setActionLoading(prev => ({ ...prev, [key]: true }));
    try {
      await client!.agent.setUserFreeAccess(email, !currentFreeAccess);
      await getUsers();
    } catch (error) {
      console.error("Failed to toggle free access:", error);
    } finally {
      setActionLoading(prev => ({ ...prev, [key]: false }));
    }
  };

  const handleSetCredits = async (email: string) => {
    const amount = parseFloat(creditAmounts[email] || "0");
    if (isNaN(amount) || amount < 0) return;

    const key = `credits-${email}`;
    setActionLoading(prev => ({ ...prev, [key]: true }));
    try {
      await client!.agent.setUserCredits(email, amount);
      setCreditAmounts(prev => ({ ...prev, [email]: "" }));
      await getUsers();
    } catch (error) {
      console.error("Failed to set credits:", error);
    } finally {
      setActionLoading(prev => ({ ...prev, [key]: false }));
    }
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
                  {/* User identity row */}
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
                        {(user as any).freeAccess && (
                          <j-badge variant="success">Free Access</j-badge>
                        )}
                      </j-flex>
                      <j-text nomargin size="300" color="ui-500" style={{ wordBreak: 'break-all', fontFamily: 'monospace' }}>
                        {user.did || 'DID not set'}
                      </j-text>
                    </j-flex>
                  </j-flex>

                  {/* Stats row */}
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

                    <j-flex direction="column" gap="100">
                      <j-text nomargin size="300" color="ui-500" weight="500">
                        CREDITS
                      </j-text>
                      <j-text nomargin size="400" weight="600">
                        {(user as any).remainingCredits === "unlimited"
                          ? "∞ Unlimited"
                          : (user as any).remainingCredits || "0"}
                      </j-text>
                    </j-flex>
                  </j-flex>

                  {/* Hosting admin controls */}
                  <j-flex gap="400" direction="column">
                    <j-box style={{ borderTop: "1px solid var(--j-color-ui-200)", paddingTop: "var(--j-space-400)" }}>
                      <j-flex gap="500" a="center" wrap="wrap">
                        {/* Free access toggle */}
                        <j-toggle
                          checked={(user as any).freeAccess}
                          disabled={actionLoading[`free-${user.email}`]}
                          onChange={() => handleToggleFreeAccess(user.email, (user as any).freeAccess)}
                        >
                          Free access
                        </j-toggle>

                        {/* Credit push */}
                        {!(user as any).freeAccess && (
                          <j-flex gap="300" a="center">
                            <j-input
                              size="sm"
                              type="number"
                              placeholder="Amount"
                              value={creditAmounts[user.email] || ""}
                              onInput={(e: any) => {
                                setCreditAmounts(prev => ({
                                  ...prev,
                                  [user.email]: e.target.value,
                                }));
                              }}
                              style={{ width: "120px" }}
                            ></j-input>
                            <j-button
                              size="sm"
                              variant="primary"
                              disabled={
                                actionLoading[`credits-${user.email}`] ||
                                !creditAmounts[user.email] ||
                                parseFloat(creditAmounts[user.email]) <= 0
                              }
                              onClick={() => handleSetCredits(user.email)}
                            >
                              {actionLoading[`credits-${user.email}`]
                                ? "Setting..."
                                : "Set Credits"}
                            </j-button>
                          </j-flex>
                        )}
                      </j-flex>
                    </j-box>
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
