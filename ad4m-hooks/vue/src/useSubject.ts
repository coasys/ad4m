import { watch, ref, shallowRef, triggerRef } from "vue";
import { SubjectRepository } from "@coasys/hooks-helpers";
import { PerspectiveProxy, LinkExpression } from "@coasys/ad4m";

export function useSubject<SubjectClass>({
  perspective,
  source,
  id,
  subject,
}: {
  perspective: PerspectiveProxy | Function;
  id?: string | Function;
  source?: string | Function;
  subject: SubjectClass;
}) {
  const idRef = typeof id === "function" ? (id as any) : ref(id);
  const sourceRef =
    typeof source === "function"
      ? (source as any)
      : ref(source || "ad4m://self");
  const perspectiveRef =
    typeof perspective === "function" ? (perspective as any) : perspective;

  let entry = ref<Record<string, any> | null>(null);
  let repo = shallowRef<SubjectRepository<any> | null>(null);

  watch(
    [perspectiveRef, sourceRef, idRef],
    async ([p, s, id]) => {
      if (p?.uuid) {
        // @ts-ignore
        const r = new SubjectRepository(subject, {
          perspective: p,
          source: s,
        });

        const res = await r.getData(id);
        repo.value = r;
        triggerRef(repo);

        subscribe(p, s);

        if (res) {
          // @ts-ignore
            entry.value = res;
        }
      }
    },
    { immediate: true }
  );

  async function fetchEntry(id: string) {
    const res = await repo.value?.getData(id);

    if (!res) return;

    entry.value = res;
  }

  async function subscribe(p: PerspectiveProxy, s: string) {
    const added = async (link: LinkExpression) => {
      const isNewEntry = link.data.source === s;
      const isUpdated = entry.value?.id === link.data.source;

      const id = isUpdated
        ? link.data.source
        : isNewEntry
          ? link.data.target
          : false;

      if (id) {
        // @ts-ignore
        const isInstance = await p.isSubjectInstance(id, new subject());

        if (isInstance) {
          fetchEntry(id);
        }
      }

      return null;
    };

    const removed = async (link: LinkExpression) => {
      // TODO: When a channel or something else attached to AD4M get removed
      // the community also thinks it's getting remove as it also point to self
      const removedEntry = link.data.source === s && s !== "ad4m://self";
      if (removedEntry) {
        const isInstance = await p.isSubjectInstance(
          link.data.source,
          // @ts-ignore
          new subject()
        );
        if (isInstance) {
          entry.value = null;
        }
      }
      return null;
    };

    // @ts-ignore
    p.addListener("link-added", added);
    // @ts-ignore
    p.addListener("link-removed", removed);

    return { added };
  }

  return { entry, repo };
}
