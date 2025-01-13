import { ref, shallowRef, triggerRef, watch } from "vue";
import { SubjectRepository } from "@coasys/hooks-helpers";
import { PerspectiveProxy, LinkExpression } from "@coasys/ad4m";

// @ts-ignore
export function useSubjects<SubjectClass>({
  perspective,
  source,
  subject,
}: {
  perspective: PerspectiveProxy | Function;
  source?: string | Function;
  subject: SubjectClass;
}) {
  const sourceRef =
    typeof source === "function"
      ? (source as any)
      : ref(source || "ad4m://self");
  const perspectiveRef =
    typeof perspective === "function" ? (perspective as any) : shallowRef(perspective);

  let entries = ref<{ [x: string]: any }[]>([]);
  let repo = shallowRef<SubjectRepository<any> | null>(null);

  watch(
    [perspectiveRef, sourceRef],
    ([p, s]) => {
      if (p?.uuid) {
        // @ts-ignore
        const rep = new SubjectRepository(subject, {
          perspective: p,
          source: s,
        });

        rep.getAllData(s).then((res) => {
          entries.value = res;
        });

        repo.value = rep;
        triggerRef(repo);

        subscribe(p, s);
      }
    },
    { immediate: true }
  );

  async function fetchEntry(id: string) {
    const entry = await repo.value?.getData(id);

    if (!entry) return;

    const isUpdatedEntry = entries.value.find((e) => e.id === entry.id);

    if (isUpdatedEntry) {
      entries.value = entries.value.map((e) => {
        const isTheUpdatedOne = e.id === isUpdatedEntry.id;
        return isTheUpdatedOne ? entry : e;
      });
    } else {
      entries.value.push(entry);
    }
  }

  async function subscribe(p: PerspectiveProxy, s: string) {
    const added = async (link: LinkExpression) => {
      const isNewEntry = link.data.source === s;
      const isUpdated = entries.value.find((e) => e.id === link.data.source);

      // @ts-ignore
      const propertyValues = typeof subject === "string" ? false : Object.values(subject.prototype.__properties);
      const includedInSubjectClassDefinition = !propertyValues || propertyValues.find((p: any) => p.through === link.data.predicate);

      const id = isNewEntry
        ? link.data.target
        : isUpdated && includedInSubjectClassDefinition
          ? link.data.source
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

    const removed = (link: LinkExpression) => {
      const removedEntry = link.data.source === s;
      if (removedEntry) {
        entries.value = entries.value.filter((e) => e.id !== link.data.target);
      }
      return null;
    };

    // @ts-ignore
    p.addListener("link-added", added);
    p.addListener("link-removed", removed);

    return { added };
  }

  return { entries, repo };
}
