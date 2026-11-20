/**
 * A run that the test "a participant process ends with the run that started it" SIGKILLs.
 *
 * Starts one participant and has it join through the API on port `argv[2]`, which accepts
 * connections and never answers: the participant then holds an open socket, as one does while
 * it joins. Prints the participant's PID and waits to be killed.
 */
import { Participant } from "./participant.js";
import type { WorldUser } from "./world.js";

const port = Number(process.argv[2]);
const participant = await Participant.start({ name: "orphan-check", did: "did:test:orphan-check" } as WorldUser);
participant
  .join({ port, token: "", neighbourhoodUrl: "neighbourhood://orphan-check", room: "orphan-check", tone: 440, tones: [440], video: null })
  .catch(() => {});
console.log(participant.pid);
setInterval(() => {}, 1 << 30);
