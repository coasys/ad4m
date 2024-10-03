import type { Address, Expression, ExpressionAdapter, PublicSharing, LanguageContext, AgentService } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import axiod from "https://deno.land/x/axiod/mod.ts";
import { PROXY_URL } from "./index.ts";
import { ExpressionGeneric } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";

export interface FileData {
  name: string;
  file_type: string;
  data_base64: string;
}

export class FileMetadata {
  name: string = "";
  size: number = 0;
  file_type: string = "";
}

export class FileExpression extends ExpressionGeneric(FileMetadata) {};

class PutAdapter implements PublicSharing {
  #agent: AgentService;

  constructor(context: LanguageContext) {
    this.#agent = context.agent;
  }

  async createPublic(fileData: FileData): Promise<Address> {
    //console.log("createPublic fileData", fileData)
    try {
        // Just in case...
        if(typeof fileData === "string"){
            //@ts-ignore
            fileData = JSON.parse(fileData)
        }
    }catch(e){}

    const data_uncompressed = Uint8Array.from(Buffer.from(fileData.data_base64, "base64"));

    const fileMetadata = {
      name: fileData.name,
      size: data_uncompressed.length,
      file_type: fileData.file_type,
      data_base64: fileData.data_base64
  } as FileMetadata

    // @ts-ignore
    const hash = UTILS.hash(JSON.stringify(fileMetadata));
    //Create the signed expression object
    const expression: FileExpression = this.#agent.createSignedExpression(fileMetadata)    

    const key = hash;
    const postData = {
      key: key,
      value: JSON.stringify(expression),
    };
    const postResult = await axiod.post(PROXY_URL, postData);
    if (postResult.status != 200) {
      console.error("Upload file data gets error: ", postResult);
    }
    
    return hash as Address;
  }
}

export default class Adapter implements ExpressionAdapter {
  putAdapter: PublicSharing;

  constructor(context: LanguageContext) {
    this.putAdapter = new PutAdapter(context);
  }

  async get(address: Address): Promise<Expression> {
    const cid = address.toString();

    let presignedUrl;
    try {
      const getPresignedUrl = await axiod.get(PROXY_URL+`?key=${cid}`);
      presignedUrl = getPresignedUrl.data.url;
    } catch (e) {
      console.error("Get File failed at getting presigned url", e);
    }

    let object;
    try {
      const getObject = await axiod.get(presignedUrl);

      object = getObject.data;
    } catch (e) {
      console.error("Get meta information failed at getting meta information", e);
    }

    return object;
  }
}

