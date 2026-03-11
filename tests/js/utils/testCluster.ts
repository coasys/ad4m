/**
 * TestCluster — utility for managing multiple AD4M executor instances in tests.
 *
 * Handles startup, readiness polling, and graceful shutdown of N executors,
 * replacing ad-hoc ChildProcess management scattered across test files.
 *
 * Usage:
 *   const cluster = new TestCluster();
 *   const node1 = await cluster.addNode({ gqlPort: 15000, hcAdminPort: 15100, ... });
 *   const node2 = await cluster.addNode({ gqlPort: 15200, hcAdminPort: 15300, ... });
 *   // ... run tests ...
 *   await cluster.shutdown();
 */

import { ChildProcess } from "node:child_process";
import { Ad4mClient } from "@coasys/ad4m";
import { apolloClient, startExecutor, gracefulShutdown, sleep } from "./utils";

export interface NodeConfig {
    gqlPort: number;
    hcAdminPort: number;
    hcAppPort: number;
    dataPath: string;
    seedPath?: string;
    adminCredential?: string;
    /** Additional flags passed to startExecutor (e.g. enableMultiUser) */
    multiUser?: boolean;
}

export interface ClusterNode {
    config: NodeConfig;
    process: ChildProcess;
    client: Ad4mClient;
    gqlPort: number;
}

export class TestCluster {
    private nodes: ClusterNode[] = [];

    /**
     * Start a new executor node and wait until its GQL endpoint is accepting connections.
     * Returns the node handle with process and client references.
     */
    async addNode(config: NodeConfig): Promise<ClusterNode> {
        const executorProcess = await startExecutor(
            config.dataPath,
            config.seedPath || "",
            config.gqlPort,
            config.hcAdminPort,
            config.hcAppPort,
            false, // languageLanguageOnly
            config.adminCredential || "",
        );

        // Wait for GQL to be reachable
        const client = await this.waitForGql(config.gqlPort, config.adminCredential || "");

        const node: ClusterNode = {
            config,
            process: executorProcess,
            client,
            gqlPort: config.gqlPort,
        };

        this.nodes.push(node);
        return node;
    }

    /**
     * Poll the GQL endpoint until it responds, with exponential backoff.
     */
    private async waitForGql(port: number, adminCredential: string, timeoutMs: number = 60000): Promise<Ad4mClient> {
        const start = Date.now();
        let lastError: Error | null = null;

        while (Date.now() - start < timeoutMs) {
            try {
                const client = new Ad4mClient(apolloClient(port, adminCredential));
                // Try a simple query to verify connectivity
                await client.runtime.info();
                return client;
            } catch (e: any) {
                lastError = e;
                await sleep(1000);
            }
        }

        throw new Error(`GQL endpoint on port ${port} not ready after ${timeoutMs}ms: ${lastError?.message}`);
    }

    /**
     * Poll the runtimeReadiness query until all subsystems are ready.
     * Falls back to runtime.info() if runtimeReadiness is not available.
     */
    async waitForReadiness(node: ClusterNode, timeoutMs: number = 120000): Promise<void> {
        const start = Date.now();

        while (Date.now() - start < timeoutMs) {
            try {
                const info = await node.client.runtime.info();
                if (info.isInitialized && info.isUnlocked) {
                    return;
                }
            } catch (e) {
                // Not ready yet
            }
            await sleep(2000);
        }

        throw new Error(`Node on port ${node.gqlPort} not fully ready after ${timeoutMs}ms`);
    }

    /**
     * Gracefully shut down all nodes in reverse order (last started → first stopped).
     */
    async shutdown(): Promise<void> {
        const shutdowns = [...this.nodes].reverse().map(async (node, i) => {
            await gracefulShutdown(node.process, `cluster node ${this.nodes.length - i}`);
        });
        await Promise.all(shutdowns);
        this.nodes = [];
    }

    /**
     * Get all running nodes.
     */
    getNodes(): ReadonlyArray<ClusterNode> {
        return this.nodes;
    }
}
