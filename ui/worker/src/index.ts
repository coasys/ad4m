/**
 * Welcome to Cloudflare Workers! This is your first worker.
 *
 * - Run `wrangler dev src/index.ts` in your terminal to start a development server
 * - Open a browser tab at http://localhost:8787/ to see your worker in action
 * - Run `wrangler publish src/index.ts --name my-worker` to publish your worker
 *
 * Learn more at https://developers.cloudflare.com/workers/
 */

import { Octokit } from "@octokit/rest";

export interface Env {
	// Example binding to KV. Learn more at https://developers.cloudflare.com/workers/runtime-apis/kv/
	// MY_KV_NAMESPACE: KVNamespace;
	//
	// Example binding to Durable Object. Learn more at https://developers.cloudflare.com/workers/runtime-apis/durable-objects/
	// MY_DURABLE_OBJECT: DurableObjectNamespace;
	//
	// Example binding to R2. Learn more at https://developers.cloudflare.com/workers/runtime-apis/r2/
	// MY_BUCKET: R2Bucket;
	//
	// Example binding to a Service. Learn more at https://developers.cloudflare.com/workers/runtime-apis/service-bindings/
	// MY_SERVICE: Fetcher;
}

export default {
	async fetch(
		request: Request,
		env: Env,
		ctx: ExecutionContext
	): Promise<Response> {
		try {
			const octokit = new Octokit();

			const { data } = await octokit.rest.repos.getLatestRelease({
				owner: 'perspect3vism',
				repo: 'ad4m',
			});

			const [_, __, ___, ____,target, version] = request.url.split('/')
			
			console.log(target, version)
	
			if (target && version) {
				if (target === 'darwin' || target === 'macos') {
					const updates = data.assets.filter((asset: any) => asset.name.includes('dmg'));
					const update = updates.find((asset: any) => !asset.name.includes('.sig'))
					const sig = updates.find((asset: any) => asset.name.includes('.sig'))

					return Response.json({
						version: data['tag_name'],
						url: update.browser_download_url,
						signature: await getSignature(sig?.browser_download_url),
					})
				} else if (target === 'linux') {
					const updates = data.assets.filter((asset: any) => asset.name.includes('deb'));
					const update = updates.find((asset: any) => !asset.name.includes('.sig'))
					const sig = updates.find((asset: any) => asset.name.includes('.sig'))
					
					return Response.json({
						version: data['tag_name'],
						url: update.browser_download_url,
						signature: await getSignature(sig?.browser_download_url),
					})
				} else if (target === 'windows') {
					const updates = data.assets.filter((asset: any) => asset.name.includes('msi'));
					const update = updates.find((asset: any) => !asset.name.includes('.sig'))
					const sig = updates.find((asset: any) => asset.name.includes('.sig'))

					return Response.json({
						version: data['tag_name'],
						url: update.browser_download_url,
						signature: await getSignature(sig?.browser_download_url),
					})
				}

				return Response.json({
					error: "invalid_platform",
					message: "The specified platform is not valid"
				}, { status: 500})
			} else {
				return new Response("", {status: 404})
			}
		} catch (e: any) {
			return new Response(e.toString(), {status: 500})
		}
	},
};

const getSignature = async (url: string) => {
	let response = await fetch(url, { redirect: 'follow' });

	let { readable, writable } = new TransformStream();

	await response.body!.pipeTo(writable);

	const reader = readable.getReader();

	const uint8array = await reader.read()

  return new TextDecoder().decode(uint8array.value)
}