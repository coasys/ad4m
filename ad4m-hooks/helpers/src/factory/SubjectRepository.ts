import {
  PerspectiveProxy,
  Link,
  Subject,
  Literal,
  LinkQuery,
} from "@coasys/ad4m";
import { setProperties } from "./model";
import { v4 as uuidv4 } from "uuid";

export const SELF = "ad4m://self";

export type ModelProps = {
  perspective: PerspectiveProxy;
  source?: string;
};

export class SubjectRepository<SubjectClass extends { [x: string]: any }> {
  source = SELF;
  subject: SubjectClass | string;
  perspective: PerspectiveProxy;
  tempSubject: any | string;

  constructor(subject: { new (): SubjectClass } | string, props: ModelProps) {
    this.perspective = props.perspective;
    this.source = props.source || this.source;
    this.subject = typeof subject === "string" ? subject : new subject();
    this.tempSubject = subject;
  }

  get className(): string {
    return typeof this.subject === "string"
      ? this.subject
      : this.subject.className;
  }

  async ensureSubject() {
    if (typeof this.tempSubject === "string") return;
    await this.perspective.ensureSDNASubjectClass(this.tempSubject);
  }

  async create(
    data: SubjectClass,
    id?: string,
    source?: string
  ): Promise<SubjectClass> {
    await this.ensureSubject();
    const base = id || Literal.from(uuidv4()).toUrl();

    let newInstance = await this.perspective.createSubject(this.subject, base);

    if (!newInstance) {
      throw "Failed to create new instance of " + this.subject;
    }

    // Connect new instance to source
    await this.perspective.add(
      new Link({
        source: source || this.source,
        predicate: "ad4m://has_child",
        target: base,
      })
    );

    Object.keys(data).forEach((key) =>
      data[key] === undefined || data[key] === null ? delete data[key] : {}
    );

    setProperties(newInstance, data);

    // @ts-ignore
    return this.getSubjectData(newInstance);
  }

  async update(id: string, data: QueryPartialEntity<SubjectClass>) {
    const instance = await this.get(id);

    if (!instance) {
      throw "Failed to find instance of " + this.subject + " with id " + id;
    }

    Object.keys(data).forEach((key) =>
      data[key] === undefined ? delete data[key] : {}
    );

    // @ts-ignore
    setProperties(instance, data);

    return this.getSubjectData(instance);
  }

  async remove(id: string) {
    if (this.perspective) {
      const linksTo = await this.perspective.get(new LinkQuery({ target: id }));
      const linksFrom = await this.perspective.get(
        new LinkQuery({ source: id })
      );
      this.perspective.removeLinks([...linksFrom, ...linksTo]);
    }
  }

  async get(id: string): Promise<SubjectClass | null> {
    if (id) {
      await this.ensureSubject();
      const subjectProxy = await this.perspective.getSubjectProxy(
        id,
        this.subject
      );

      // @ts-ignore
      return subjectProxy || null;
    } else {
      const all = await this.getAll();
      return all[0] || null;
    }
  }

  async getData(id: string): Promise<SubjectClass | string | null> {
    const entry = await this.get(id);
    if (entry) {
      // @ts-ignore
      return await this.getSubjectData(entry);
    }

    return null;
  }

  private async getSubjectData(entry: any) {
    let links = await this.perspective.get(
      new LinkQuery({ source: entry.baseExpression })
    );

    let data: any = await this.perspective.getSubjectData(this.subject, entry.baseExpression)

    for (const key in data) {
      if (this.tempSubject.prototype?.__properties[key]?.transform) {
        data[key] =
          this.tempSubject.prototype.__properties[key].transform(data[key]);
      }
    }

    return {
      id: entry.baseExpression,
      timestamp: links[0].timestamp,
      author: links[0].author,
      ...data,
    }
  }

  async getAll(source?: string, query?: QueryOptions): Promise<SubjectClass[]> {
    await this.ensureSubject();

    const tempSource = source || this.source;

    let res = [];

    if (query) {
      try {
        const queryResponse = (
          await this.perspective.infer(
            `findall([Timestamp, Base], (subject_class("${this.className}", C), instance(C, Base), link("${tempSource}", Predicate, Base, Timestamp, Author)), AllData), length(AllData, DataLength), sort(AllData, SortedData).`
          )
        )[0];

        if (queryResponse.SortedData >= query.size) {
          const isOutofBound =
            query.size * query.page > queryResponse.DataLength;

          const newPageSize = isOutofBound
            ? queryResponse.DataLength - query.size * (query.page - 1)
            : query.size;

          const mainQuery = `findall([Timestamp, Base], (subject_class("${this.className}", C), instance(C, Base), link("${tempSource}", Predicate, Base, Timestamp, Author)), AllData), sort(AllData, SortedData), reverse(SortedData, ReverseSortedData), paginate(ReverseSortedData, ${query.page}, ${newPageSize}, PageData).`;
          res = await this.perspective.infer(mainQuery);

          //@ts-ignore
          res = res[0].PageData.map((r) => ({
            Base: r[1],
            Timestamp: r[0],
          }));
        } else {
          res = await this.perspective.infer(
            `subject_class("${this.className}", C), instance(C, Base), triple("${tempSource}", Predicate, Base).`
          );
        }
      } catch (e) {
        console.log("Query failed", e);
      }
    } else {
      res = await this.perspective.infer(
        `subject_class("${this.className}", C), instance(C, Base), triple("${tempSource}", Predicate, Base).`
      );
    }

    const results =
      res &&
      res.filter(
        //@ts-ignore
        (obj, index, self) => index === self.findIndex((t) => t.Base === obj.Base)
      );

    if (!res) return [];

    const data = await Promise.all(
      results.map(async (result: any) => {
        let subject = new Subject(
          this.perspective!,
          //@ts-ignore
          result.Base,
          this.className
        );

        await subject.init();

        return subject;
      })
    );

    // @ts-ignore
    return data;
  }

  async getAllData(
    source?: string,
    query?: QueryOptions
  ): Promise<SubjectClass[]> {
    const subjects = await this.getAll(source, query);

    const entries = await Promise.all(
      subjects.map((e) => this.getSubjectData(e))
    );

    // @ts-ignore
    return entries;
  }
}

export type QueryPartialEntity<T> = {
  [P in keyof T]?: T[P] | (() => string);
};

export type QueryOptions = {
  page: number;
  size: number;
  infinite: boolean;
  uniqueKey: string;
};
