// Static tool definitions for the AD4M executor MCP surface.
// Captured verbatim from ad4m-executor tools/list (feat/static-instance-tools,
// dynamicClassTools=false) so the plugin can register all agent tools
// synchronously at register() time. OpenClaw builds the agent tool surface
// from a cold load that only runs register(); tools registered later from
// the bridge service never become visible to sessions.
//
// To refresh: run an executor with --enable-mcp true, call tools/list, and
// paste the name/description/inputSchema of every tool named in
// STATIC_MCP_TOOLS (index.ts). Keep schemas byte-for-byte as the executor
// emits them (schemars 1.0, draft 2020-12).
import type { McpTool } from "./types";

export const STATIC_TOOL_DEFS: McpTool[] = 
[
  {
    "name": "get_my_did",
    "description": "Get the DID (Decentralized Identifier) of the current agent. Use this to identify your own messages when filtering \u2014 compare the 'author' field in message data against your DID.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "GetAgentProfileParams",
      "description": "Parameters for getting the agent's public profile",
      "type": "object"
    }
  },
  {
    "name": "auth_status",
    "description": "Check the current authentication status of the MCP session.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "AuthStatusParams",
      "description": "Parameters for checking authentication status (no params needed)",
      "type": "object"
    }
  },
  {
    "name": "login_email",
    "description": "Login to a multi-user AD4M executor using email and password. Returns a JWT token on success that will be used for subsequent operations.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "LoginEmailParams",
      "description": "Parameters for email/password login (multi-user mode)",
      "type": "object",
      "properties": {
        "email": {
          "description": "User email address",
          "type": "string"
        },
        "password": {
          "description": "User password",
          "type": "string"
        }
      },
      "required": [
        "email",
        "password"
      ]
    }
  },
  {
    "name": "set_agent_profile",
    "description": "Set the current agent's public profile (username, name, bio, email). These fields are visible to other agents and Flux users in neighbourhoods. Only provided fields are updated; omitted fields keep their current values.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "SetAgentProfileParams",
      "description": "Parameters for setting the agent's profile",
      "type": "object",
      "properties": {
        "username": {
          "description": "Display username",
          "type": "string",
          "nullable": true
        },
        "given_name": {
          "description": "Given (first) name",
          "type": "string",
          "nullable": true
        },
        "family_name": {
          "description": "Family (last) name",
          "type": "string",
          "nullable": true
        },
        "email": {
          "description": "Email address",
          "type": "string",
          "nullable": true
        },
        "bio": {
          "description": "Bio/description text",
          "type": "string",
          "nullable": true
        }
      }
    }
  },
  {
    "name": "signup",
    "description": "Create a new user account (multi-user mode). Sends a verification email with a code. Use verify_email_code to complete signup.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "SignupParams",
      "description": "Parameters for user signup (multi-user mode)",
      "type": "object",
      "properties": {
        "email": {
          "description": "User email address",
          "type": "string"
        },
        "password": {
          "description": "User password",
          "type": "string"
        }
      },
      "required": [
        "email",
        "password"
      ]
    }
  },
  {
    "name": "verify_email_code",
    "description": "Verify an email code to complete signup or login. Returns a JWT token on success. The verification_type must be 'signup' or 'login'.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "VerifyEmailCodeParams",
      "description": "Parameters for verifying an email code (multi-user mode)",
      "type": "object",
      "properties": {
        "email": {
          "description": "User email address",
          "type": "string"
        },
        "code": {
          "description": "6-digit verification code",
          "type": "string"
        },
        "verification_type": {
          "description": "Type: \"signup\" or \"login\"",
          "type": "string"
        }
      },
      "required": [
        "email",
        "code",
        "verification_type"
      ]
    }
  },
  {
    "name": "list_perspectives",
    "description": "List all AD4M perspectives. A perspective is a subjective graph database \u2014 a personal collection of links (RDF-like triples: source \u2192 predicate \u2192 target) that can be queried, modified, and optionally shared as a 'neighbourhood' for real-time P2P collaboration. Each has a UUID and a human-readable name.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "ListPerspectivesParams",
      "description": "Parameters for listing perspectives",
      "type": "object"
    }
  },
  {
    "name": "add_perspective",
    "description": "Create a new perspective (local knowledge graph). Returns the UUID. You can then add links, register models (subject classes), and create typed instances within it. To share it for collaboration, convert it to a neighbourhood.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "AddPerspectiveParams",
      "description": "Parameters for creating a new perspective",
      "type": "object",
      "properties": {
        "name": {
          "description": "Name for the new perspective",
          "type": "string"
        }
      },
      "required": [
        "name"
      ]
    }
  },
  {
    "name": "add_model",
    "description": "Register a model (subject class) using a SHACL JSON definition. This defines the schema \u2014 properties, collections, types \u2014 for typed objects in the perspective. Once registered, the class appears in describe_perspective and can be used with the generic instance_* tools by class_name (instance_create, instance_query, \u2026). If the executor runs with dynamicClassTools enabled, per-class tools ({class}_create, {class}_set_{property}, \u2026) are additionally generated and the tool list updates after registration.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "AddModelParams",
      "description": "Parameters for adding SDNA (subject class definition) to a perspective",
      "type": "object",
      "properties": {
        "perspective_id": {
          "description": "Perspective UUID",
          "type": "string"
        },
        "class_name": {
          "description": "Bare subject class name, matching the local name of the shape's\n`target_class` (e.g. `Task` for `target_class: \"board://Task\"`)",
          "type": "string"
        },
        "shacl_json": {
          "description": "SHACL shape definition as JSON string",
          "type": "string"
        }
      },
      "required": [
        "perspective_id",
        "class_name",
        "shacl_json"
      ]
    }
  },
  {
    "name": "neighbourhood_join_from_url",
    "description": "Join an existing neighbourhood by URL. Creates a local perspective that syncs with the shared neighbourhood. Returns the perspective UUID for interacting with the neighbourhood's data.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "NeighbourhoodJoinParams",
      "description": "Parameters for joining a neighbourhood",
      "type": "object",
      "properties": {
        "url": {
          "description": "Neighbourhood URL to join (e.g. neighbourhood://Qm...)",
          "type": "string"
        }
      },
      "required": [
        "url"
      ]
    }
  },
  {
    "name": "neighbourhood_publish_from_perspective",
    "description": "Publish a local perspective as a shared neighbourhood. Automatically clones the given link language template to create a unique sync instance. Returns the neighbourhood URL that others can use to join via `neighbourhood_join_from_url`. Use `list_link_language_templates` first to find available templates.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "NeighbourhoodPublishParams",
      "description": "Parameters for publishing a perspective as a neighbourhood",
      "type": "object",
      "properties": {
        "perspective_uuid": {
          "description": "UUID of the local perspective to publish as a shared neighbourhood",
          "type": "string"
        },
        "link_language": {
          "description": "Address of a link language to use for this neighbourhood.\nCan be a template address (will be cloned) or an already-cloned language.\nUse `list_link_language_templates` to see available templates.",
          "type": "string"
        },
        "name": {
          "description": "Optional human-readable name for this neighbourhood (used as the cloned language name).\nIf not provided, a default name will be generated.",
          "type": "string",
          "default": "Neighbourhood"
        }
      },
      "required": [
        "perspective_uuid",
        "link_language"
      ]
    }
  },
  {
    "name": "list_link_language_templates",
    "description": "List available link language templates that can be used when publishing a neighbourhood. Each template is a P2P synchronization engine. Returns address, name, and description for each template. Pass the address as `link_language` when calling `neighbourhood_publish_from_perspective`.",
    "inputSchema": {
      "type": "object",
      "properties": {}
    }
  },
  {
    "name": "add_link",
    "description": "Add a link (RDF-like triple) to a perspective. Links are the fundamental data unit \u2014 all data (properties, type markers, collections) is stored as links. Example: source='did:key:abc' predicate='ad4m://name' target='literal://string:Alice'. In shared neighbourhoods, links sync to all members.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "AddLinkParams",
      "description": "Parameters for adding a link to a perspective",
      "type": "object",
      "properties": {
        "perspective_id": {
          "description": "Perspective UUID",
          "type": "string"
        },
        "source": {
          "description": "Link source URI",
          "type": "string"
        },
        "predicate": {
          "description": "Link predicate URI",
          "type": "string"
        },
        "target": {
          "description": "Link target URI",
          "type": "string"
        }
      },
      "required": [
        "perspective_id",
        "source",
        "predicate",
        "target"
      ]
    }
  },
  {
    "name": "query_links",
    "description": "Query links in a perspective. Links are RDF-like triples with source, predicate, and target. Filter by any combination \u2014 omit a filter to match all values for that field. Example: source='expr://abc' with no predicate/target returns all links from that address. Use predicate filter to find specific property values.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "QueryLinksParams",
      "description": "Parameters for querying links in a perspective",
      "type": "object",
      "properties": {
        "perspective_id": {
          "description": "Perspective UUID",
          "type": "string"
        },
        "source": {
          "description": "Optional source URI filter",
          "type": "string",
          "nullable": true
        },
        "predicate": {
          "description": "Optional predicate URI filter",
          "type": "string",
          "nullable": true
        },
        "target": {
          "description": "Optional target URI filter",
          "type": "string",
          "nullable": true
        }
      },
      "required": [
        "perspective_id"
      ]
    }
  },
  {
    "name": "describe_perspective",
    "description": "Describe the data model of a perspective: every registered subject class (model) with its properties (name, type, required, cardinality, hints), its collections, and any flows (state machines). Call this right after list_perspectives / neighbourhood_join_from_url \u2014 it returns the schema as data so you can then use the generic instance_create / instance_query / instance_get / instance_update / instance_add_to_collection / instance_remove_from_collection / instance_remove / instance_transcript tools with class_name set to one of the returned class names. Property values passed to those tools are validated against this schema.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "DescribePerspectiveParams",
      "description": "Parameters for describing a perspective's data model",
      "type": "object",
      "properties": {
        "perspective_id": {
          "description": "Perspective UUID",
          "type": "string"
        }
      },
      "required": [
        "perspective_id"
      ]
    }
  },
  {
    "name": "instance_create",
    "description": "Create a new instance of a subject class. class_name is one of the class names from describe_perspective; properties is a JSON object of property values (single JSON value per scalar property, array of item URIs per collection). Required properties must be present; every value is validated against the class schema (property, expected type, cardinality are named on rejection). Optionally pass parent to also link the instance as an ad4m://has_child child of another instance (e.g. a Message into a Channel). Returns the new instance's base_uri (its id). Example: instance_create(perspective_id, class_name='Message', properties={\"body\": \"Hello\"}, parent='<channel uri>').",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "InstanceCreateParams",
      "description": "Parameters for creating a subject instance of any class",
      "type": "object",
      "properties": {
        "perspective_id": {
          "description": "Perspective UUID",
          "type": "string"
        },
        "class_name": {
          "description": "Subject class name exactly as listed by describe_perspective (e.g. \"Message\")",
          "type": "string"
        },
        "properties": {
          "description": "Property values keyed by property name. Scalar properties take one JSON\nvalue (string / number / boolean, matching the property's type);\ncollection properties take an array of item URIs. Required properties\nmust be present.",
          "type": "object",
          "additionalProperties": true,
          "nullable": true
        },
        "base_uri": {
          "description": "Optional URI for the new instance. A random one is generated when omitted.",
          "type": "string",
          "nullable": true
        },
        "parent": {
          "description": "Optional parent URI. The new instance is additionally linked as an\n`ad4m://has_child` child of this node (e.g. a Message inside a Channel).",
          "type": "string",
          "nullable": true
        }
      },
      "required": [
        "perspective_id",
        "class_name"
      ]
    }
  },
  {
    "name": "instance_query",
    "description": "List instances of a subject class with their property values. class_name is one of the class names from describe_perspective. Optional filter is a where clause on property values: exact match {\"status\": \"open\"}, IN {\"status\": [\"open\", \"doing\"]}, operators {\"count\": {\"gt\": 5}} / {\"title\": {\"contains\": \"mcp\"}} / {\"owner\": {\"not\": \"\u2026\"}}, combinators \"OR\" / \"AND\" / \"NOT\"; \"id\" filters on the instance URI. Optional parent restricts to ad4m://has_child children of one instance (e.g. messages of a channel). Paginate with limit (default 100) and offset; total_count reports the full match count. Each instance has id (its base_uri), author, timestamp, and one key per property.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "InstanceQueryParams",
      "description": "Parameters for querying instances of a class",
      "type": "object",
      "properties": {
        "perspective_id": {
          "description": "Perspective UUID",
          "type": "string"
        },
        "class_name": {
          "description": "Subject class name as listed by describe_perspective",
          "type": "string"
        },
        "filter": {
          "description": "Optional filter on property values (a model-query `where` clause).\nKeys are property names. Values: an exact match (`{\"status\": \"open\"}`),\nan array for IN (`{\"status\": [\"open\", \"doing\"]}`), or an operator object\n(`{\"count\": {\"gt\": 5}}`, `{\"title\": {\"contains\": \"mcp\"}}`,\n`{\"owner\": {\"not\": \"did:key:\u2026\"}}`). Combine with `\"OR\": [..]` /\n`\"AND\": [..]` / `\"NOT\": {..}`. `id` filters on the instance URI.",
          "type": "object",
          "additionalProperties": true,
          "nullable": true
        },
        "parent": {
          "description": "Optional parent URI: only return instances that are `ad4m://has_child`\nchildren of this node (e.g. the messages of one channel).",
          "type": "string",
          "nullable": true
        },
        "limit": {
          "description": "Maximum number of instances to return (default 100).",
          "type": "integer",
          "format": "uint",
          "minimum": 0,
          "nullable": true
        },
        "offset": {
          "description": "Number of instances to skip, for pagination.",
          "type": "integer",
          "format": "uint",
          "minimum": 0,
          "nullable": true
        }
      },
      "required": [
        "perspective_id",
        "class_name"
      ]
    }
  },
  {
    "name": "instance_get",
    "description": "Get one instance of a subject class by its base_uri, with all property values and collections resolved. class_name is one of the class names from describe_perspective. Returns an error if no instance of that class exists at the URI.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "InstanceGetParams",
      "description": "Parameters for reading one instance",
      "type": "object",
      "properties": {
        "perspective_id": {
          "description": "Perspective UUID",
          "type": "string"
        },
        "class_name": {
          "description": "Subject class name as listed by describe_perspective",
          "type": "string"
        },
        "base_uri": {
          "description": "URI of the instance (the `id` returned by instance_create / instance_query)",
          "type": "string"
        }
      },
      "required": [
        "perspective_id",
        "class_name",
        "base_uri"
      ]
    }
  },
  {
    "name": "instance_update",
    "description": "Set one or more single-valued properties on an existing instance. class_name is one of the class names from describe_perspective; properties is a JSON object of the values to change (unlisted properties are untouched). Values are validated against the class schema \u2014 the error names the property, expected type and cardinality. Collections cannot be set here: use instance_add_to_collection. Example: instance_update(perspective_id, class_name='Task', base_uri='<id>', properties={\"status\": \"done\"}).",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "InstanceUpdateParams",
      "description": "Parameters for updating scalar properties of an instance",
      "type": "object",
      "properties": {
        "perspective_id": {
          "description": "Perspective UUID",
          "type": "string"
        },
        "class_name": {
          "description": "Subject class name as listed by describe_perspective",
          "type": "string"
        },
        "base_uri": {
          "description": "URI of the instance to update",
          "type": "string"
        },
        "properties": {
          "description": "Property values to set, keyed by property name. Only the given\nproperties change; each must be a single-valued property of the class.",
          "type": "object",
          "additionalProperties": true
        }
      },
      "required": [
        "perspective_id",
        "class_name",
        "base_uri",
        "properties"
      ]
    }
  },
  {
    "name": "instance_add_to_collection",
    "description": "Add an item to a collection property of an instance (e.g. add a Message to a Channel's messages). class_name is the owning instance's class, base_uri its id, collection one of the names listed under collections by describe_perspective, item_uri the URI of the item (usually another instance's id). Adding the same item twice is a no-op. Undo with instance_remove_from_collection.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "InstanceAddToCollectionParams",
      "description": "Parameters for adding an item to a collection property",
      "type": "object",
      "properties": {
        "perspective_id": {
          "description": "Perspective UUID",
          "type": "string"
        },
        "class_name": {
          "description": "Subject class name of the instance that owns the collection",
          "type": "string"
        },
        "base_uri": {
          "description": "URI of the instance that owns the collection",
          "type": "string"
        },
        "collection": {
          "description": "Collection property name as listed under `collections` by describe_perspective",
          "type": "string"
        },
        "item_uri": {
          "description": "URI of the item to add (typically another instance's `id`)",
          "type": "string"
        }
      },
      "required": [
        "perspective_id",
        "class_name",
        "base_uri",
        "collection",
        "item_uri"
      ]
    }
  },
  {
    "name": "instance_remove_from_collection",
    "description": "Remove an item from a collection property of an instance (e.g. take a Message out of a Channel's messages). class_name is the owning instance's class, base_uri its id, collection one of the names listed under collections by describe_perspective, item_uri the item to remove as listed by instance_get. Only the membership link is removed \u2014 the item itself is untouched (use instance_remove to delete it). Removing an item that is not in the collection is a no-op (links_removed: 0).",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "InstanceRemoveFromCollectionParams",
      "description": "Parameters for removing an item from a collection property",
      "type": "object",
      "properties": {
        "perspective_id": {
          "description": "Perspective UUID",
          "type": "string"
        },
        "class_name": {
          "description": "Subject class name of the instance that owns the collection",
          "type": "string"
        },
        "base_uri": {
          "description": "URI of the instance that owns the collection",
          "type": "string"
        },
        "collection": {
          "description": "Collection property name as listed under `collections` by describe_perspective",
          "type": "string"
        },
        "item_uri": {
          "description": "URI of the item to remove, exactly as instance_get lists it in the collection",
          "type": "string"
        }
      },
      "required": [
        "perspective_id",
        "class_name",
        "base_uri",
        "collection",
        "item_uri"
      ]
    }
  },
  {
    "name": "instance_remove",
    "description": "Delete an instance of a subject class: removes all its property links, its type markers, and every inbound link from other instances (e.g. collection membership). Refuses if no instance of class_name exists at base_uri, so a wrong class or URI cannot delete something else. \u26a0\ufe0f Irreversible.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "InstanceRemoveParams",
      "description": "Parameters for removing an instance",
      "type": "object",
      "properties": {
        "perspective_id": {
          "description": "Perspective UUID",
          "type": "string"
        },
        "class_name": {
          "description": "Subject class name of the instance",
          "type": "string"
        },
        "base_uri": {
          "description": "URI of the instance to remove",
          "type": "string"
        }
      },
      "required": [
        "perspective_id",
        "class_name",
        "base_uri"
      ]
    }
  },
  {
    "name": "instance_transcript",
    "description": "Read the most recent instances of a class that are ad4m://has_child children of a node, as a plain-text transcript in chronological order \u2014 one entry per instance with its timestamp, author display name and DID, and its text property (body by default). Ideal for reading a Flux channel (class_name='Message', parent=<channel id>) in one call. limit picks how many of the newest to show (default 50); when there are more, the output starts with '(showing last N of M \u2026)'. Use instance_query for the full property maps or for filters.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "InstanceTranscriptParams",
      "description": "Parameters for reading a transcript of one class's children under a node",
      "type": "object",
      "properties": {
        "perspective_id": {
          "description": "Perspective UUID",
          "type": "string"
        },
        "class_name": {
          "description": "Subject class of the children to read (e.g. \"Message\"), as listed by\ndescribe_perspective",
          "type": "string"
        },
        "parent": {
          "description": "Parent node URI whose ad4m://has_child children are read (e.g. a\nChannel's id). A bare string is wrapped as a literal URI.",
          "type": "string"
        },
        "limit": {
          "description": "How many of the most recent instances to include (default 50, max 500)",
          "type": "integer",
          "format": "uint",
          "minimum": 0,
          "nullable": true
        },
        "text_property": {
          "description": "Which property holds the text shown per entry. Defaults to `body`,\nfalling back to the class's identity property.",
          "type": "string",
          "nullable": true
        }
      },
      "required": [
        "perspective_id",
        "class_name",
        "parent"
      ]
    }
  },
  {
    "name": "add_child",
    "description": "Link a child node under a parent with ad4m://has_child \u2014 the generic tree Flux uses for messages in channels, channels under ad4m://self, tasks in boards. Class-agnostic: neither node needs to be a subject-class instance. Bare strings are wrapped as literal URIs. Prefer instance_create(parent=\u2026) when creating a new instance, and instance_add_to_collection when the parent's class declares the collection; use this for nodes that are not instances (e.g. parent='ad4m://self') or to re-parent an existing instance.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "AddChildParams",
      "description": "Parameters for adding a child to a parent node",
      "type": "object",
      "properties": {
        "perspective_id": {
          "description": "Perspective UUID",
          "type": "string"
        },
        "parent": {
          "description": "Parent node URI (e.g. a Channel's id, or `ad4m://self` for the\nperspective root). A bare string is wrapped as a literal URI.",
          "type": "string"
        },
        "child": {
          "description": "Child node URI (e.g. a Message's id). A bare string is wrapped as a\nliteral URI.",
          "type": "string"
        }
      },
      "required": [
        "perspective_id",
        "parent",
        "child"
      ]
    }
  },
  {
    "name": "get_children",
    "description": "List the children of a node linked via ad4m://has_child, regardless of their class: id, timestamp and author of each child link, oldest first. Class-agnostic \u2014 works for ad4m://self (the perspective root, whose children are the top-level channels), for plain-string ids, and for instances. limit keeps the most recent N (default 100, max 500); total_count is the full number. To read children of one class with their property values use instance_query(class_name, parent) or instance_transcript.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "GetChildrenParams",
      "description": "Parameters for listing the children of a parent node",
      "type": "object",
      "properties": {
        "perspective_id": {
          "description": "Perspective UUID",
          "type": "string"
        },
        "parent": {
          "description": "Parent node URI to list the children of. A bare string is wrapped as\na literal URI.",
          "type": "string"
        },
        "limit": {
          "description": "Maximum number of children to return \u2014 the most recent ones, in\nchronological order (default 100, max 500). total_count reports how\nmany there are in all.",
          "type": "integer",
          "format": "uint",
          "minimum": 0,
          "nullable": true
        }
      },
      "required": [
        "perspective_id",
        "parent"
      ]
    }
  },
  {
    "name": "get_documentation",
    "description": "Read the AD4M executor's documentation as markdown. topic='overview' explains what AD4M is, the static tool surface (describe_perspective + instance_*), the workflow and the rules for writing data other agents and humans can use \u2014 call it first if you are new to AD4M. topic='architecture' covers perspectives, links, neighbourhoods and the SHACL class format in depth. No authentication needed.",
    "inputSchema": {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "title": "GetDocumentationParams",
      "description": "Parameters for reading the executor's documentation",
      "type": "object",
      "properties": {
        "topic": {
          "description": "Which document: \"overview\" (start here) or \"architecture\"",
          "$ref": "#/$defs/DocTopic"
        }
      },
      "required": [
        "topic"
      ],
      "$defs": {
        "DocTopic": {
          "description": "Which document to return.",
          "oneOf": [
            {
              "description": "What AD4M is, the static tool surface, the workflow, and the rules\nthat keep data usable by humans and other agents. Start here.",
              "type": "string",
              "const": "overview"
            },
            {
              "description": "Perspectives, links, languages, neighbourhoods, and the SHACL subject\nclass (social DNA) format in detail.",
              "type": "string",
              "const": "architecture"
            }
          ]
        }
      }
    }
  }
];