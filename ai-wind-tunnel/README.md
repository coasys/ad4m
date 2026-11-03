# AI wind tunnel

Real model servers, through a real executor, on the machine at hand. The wind tunnel runs
every cell of **providers × services**, checks that each answer is right, measures it, and
writes the result as evidence. Recommendations of which runtime and model to use on which
hardware are derived from the evidence and only from it.

| Path | What |
|---|---|
| `matrix.json` | The cells: per provider and service, the models (a manifest `modelId` or the provider's own name) and the reason a provider cannot serve a service. |
| `../rust-executor/src/ai_service/local/manifest.json` | Runtimes (llama-server, whisper-server, nemo-speech, sherpa-onnx builds per `<os>-<arch>-<backend>`) and model files, with URL, sha256, size, licence and memory. The executor's local AI setup will install from it; the wind tunnel installs from it the same way. |
| (manifest `memoryBytes`) | What serving a model takes: the peak GPU memory (CPU-only servers: resident memory) measured on the reference run with the cell's `serverFlags` (llama-server: 8k context, 4 slots), rounded up to 0.1 GB. The fit rule and the local setup read it. |
| `hardware-classes.json` | Ordered rules from a machine (OS, arch, RAM, GPUs, usable backends) to its hardware class. |
| `hardware-class-fixtures.json` | Machines and the class each must get. Every classifier (TS here, Rust in the executor) runs them. |
| `evidence/<class>/<date>-<fingerprint>.json` | One file per run: the fingerprint, every cell's status, checks and metrics. Schema v1. |
| `recommendations.json` | Per class and service, the recommended provider and model, the metrics that justify it and the evidence files that hold them. Derived; never edit it by hand. |
| `fixtures/` | The retrieval set, the STT recordings with their transcripts and the TTS texts. |

The code lives in `tests/js/helpers/ai-wind-tunnel/`; its unit tests in
`tests/js/tests/ai-wind-tunnel.unit.test.ts`.

## The matrix

| | llm | completion | embeddings | stt | tts |
|---|---|---|---|---|---|
| **ollama** | qwen3.5 0.8b/2b/4b | qwen3.5:0.8b | nomic-embed-text | unsupported | unsupported |
| **llama-server** (llama.cpp) | Qwen3.5 0.8B Q8_0, 2B/4B Q4_K_M | 0.8B | nomic-embed-text v1.5 Q8_0 | Qwen3-ASR 0.6B/1.7B Q8_0, Voxtral Mini 3B Q4_K_M (through a front) | unsupported (no speech route in b11669) |
| **vllm** | Qwen/Qwen3.5-0.8B | same | | | unsupported |
| **whisper-server** (whisper.cpp) | unsupported | unsupported | unsupported | whisper base.en, small (q5_1) | unsupported |
| **speaches** | unsupported | unsupported | unsupported | faster-whisper base.en, small | Kokoro 82M |
| **nemo-speech** (NeMo-Speech.cpp) | unsupported | unsupported | unsupported | Parakeet TDT 0.6B v3, Nemotron 3.5 ASR streaming 0.6B (Q8_0) | |
| **sherpa-onnx** | unsupported | unsupported | unsupported | Parakeet TDT 0.6B v3 int8 (through a front) | unsupported (WebSocket only) |

A cell is one of:

- **pass**: every check held.
- **fail**: a check did not hold, or the server or the executor failed. The reason names it.
- **unsupported**: the provider cannot serve the service at all (from `matrix.json`).
- **skipped**: the provider is not available on this machine: no binary, no docker, no GPU
  for vLLM, or a GPU stack that does not start here. The reason says which.

## What each cell checks

Every call goes through the executor exactly as an app's would: the endpoint and the model
are registered with `ai.addEndpoint` and `ai.addModel` (Ollama models are pulled with
`ai.pullModel`, which is part of the test), then requests go through the SDK's `ai.*` RPCs
and the `/v1` OpenAI-compatible API.

| Service | Checks (all must hold) | Metrics |
|---|---|---|
| llm | `/v1/chat/completions` answers "Paris" for the capital of France; the same through an SDK task (`ai.addTask` + `ai.prompt`); a tool call names `get_weather` with JSON arguments naming Paris; a stream yields more than one chunk and the right text | time to first token, decode tokens/s, prefill tokens/s (a ~600-token prompt with a nonce in front, so no prompt cache helps), short-answer latency, throughput at concurrency 1 and 4 |
| completion | `/v1/completions` continues "The capital city of France is" with Paris | latency |
| embeddings | a batch and single texts embed; one dimension throughout; `ai.embed` over the SDK matches `/v1` (cosine > 0.99); every query of `fixtures/retrieval.json` ranks its passage first | texts/s and latency for a batch of 32, single-text latency, MRR |
| stt | each recording of `fixtures/audio.json` transcribes under its word error rate limit, over `/v1/audio/transcriptions` and over the SDK's transcription stream (`ai.openTranscriptionStream` + feed, which sends no language hint) | WER per clip and mean (`werMean`, `streamWerMean`), real-time factor, per-utterance latency, stream latency; on the noisy clip with no hint, the language the server reports (`noisyLanguage`) and its WER |
| tts | `/v1/audio/speech` returns WAV that decodes, is not silent and lasts as long as its words need; the audio transcribed by the best STT model that passed in the same run comes back under the WER limit | real-time factor, time to first audio byte, latency, round-trip WER |

Every cell also records the first request's time (a cold model loads then), the server's
start-to-ready time when the run started it, the peak resident memory of the server's
process tree and the GPU memory its DRM clients hold (amdgpu `fdinfo`, CUDA through
`nvidia-smi`), the executor's view of the endpoint's health, and whether the model fits the
machine: model memory ≤ min(GPU budget − 1 GiB, RAM − 4 GiB). With direct calls on (the
default) the same request also goes straight to the provider, and `executorOverheadMs` is the
difference of the medians (each timed prompt carries its own nonce and the order of the two
calls alternates, so neither rides on the other's prompt cache). Each measurement is repeated (`--repeat`, default 3) and reported
as median, p95, min, max and mean.

WER normalises case, punctuation, hyphens and whole numbers ("9" and "nine" are one word).
The clean clips do not separate STT models (every one scores 0), so one clip is also played
with seeded white noise above its level (`noise` in `fixtures/audio.json`, added at load
time); STT models rank on the mean WER over all clips (`werMean`).

## Running it on a new machine

Needs: Node 24+, `pnpm`, `curl`, `ffmpeg` (decodes the m4a recording), an `ad4m-executor`
binary, and whichever providers you want to measure. Nothing has to be installed for
llama-server, whisper-server, nemo-speech and sherpa-onnx: the run downloads the manifest's
build for the machine and checks its sha256. Ollama needs its binary; speaches and vLLM need docker.

```bash
cd tests/js
pnpm install
pnpm --dir ../../core run build           # the SDK the run uses
export EXECUTOR_PATH=/path/to/ad4m-executor   # else ../../target/release/ad4m-executor

pnpm run ai-wind-tunnel -- --dry-run      # the matrix and this machine's class
pnpm run ai-wind-tunnel                   # everything, repeat 3
pnpm run ai-wind-tunnel -- --providers ollama,llama-server --services llm,embeddings --repeat 5
```

Options: `--providers`, `--services`, `--models` (manifest ids or provider names),
`--repeat N`, `--concurrency 1,4`, `--llama-builds rocm,vulkan` (manifest build keys or
backends; default the GPU builds, or CPU without a GPU backend), `--no-direct`, `--attach`,
`--out FILE`, `--no-write`.

A full run takes a while the first time (about 12 GB of models); afterwards the cache
(`~/.cache/ad4m-ai-wind-tunnel`, or `AI_WT_CACHE`) holds runtimes, model files, Ollama's
models and speaches' Hugging Face cache. `AI_WT_FRESH=1` gives Ollama an empty model
directory, so its pull is measured from the network.

### Providers already running

Each provider can attach to a server instead of starting one. `--attach` uses only these
and starts nothing:

| Variable | Provider |
|---|---|
| `AI_WT_OLLAMA_URL` | Ollama |
| `AI_WT_LLAMA_SERVER_URL` (+ `AI_WT_LLAMA_BACKEND`) | llama-server; cells use the model it serves |
| `AI_WT_WHISPER_SERVER_URL` | whisper-server started with `--request-path /v1/audio --inference-path /transcriptions` |
| `AI_WT_SPEACHES_URL` | speaches |
| `AI_WT_VLLM_URL` | vLLM |
| `AI_WT_NEMO_SPEECH_URL` (+ `AI_WT_NEMO_SPEECH_BACKEND`) | `nemo-speech serve` |

Other knobs: `AI_WT_OLLAMA_BIN`, `AI_WT_LLAMA_SERVER` / `AI_WT_WHISPER_SERVER` /
`AI_WT_NEMO_SPEECH` / `AI_WT_SHERPA_ONNX` (a binary instead of the manifest's build),
`AI_WT_LLAMA_STT_ROUTE=native` (llama-server STT cells on its own transcription route, no front), `AI_WT_SPEACHES_IMAGE`, `AI_WT_VLLM_IMAGE` (CUDA),
`AI_WT_VLLM_ROCM_IMAGE` (an image built for the machine's AMD GPU; there is no official one
for consumer and APU parts), `AI_WT_VLLM_GPU_UTIL`, `AI_WT_CELL_TIMEOUT_MS`,
`AI_WT_RESERVED_PORTS` (ports the run never binds or attaches to), `AI_WT_CONTAINER_MEMORY`
(a hard cap, e.g. `8g`, on every container's memory and swap).

The run binds only free ports on 127.0.0.1, removes only the containers it named
(`ai-wt-<run id>-...`) and stops only processes it started, also on Ctrl-C.

### whisper-server and the OpenAI path

whisper-server answers `POST /inference` by default. Started with
`--request-path /v1/audio --inference-path /transcriptions` it answers
`POST /v1/audio/transcriptions` instead (and `/inference` is gone), which is the path the
executor's transcription client posts to. It is registered as an `OPEN_AI_AUDIO` endpoint. It
lists no `/v1/models`, so the executor's health check does not see it as reachable;
transcription works regardless (the evidence records the endpoint health per cell).

### Speech to text beside whisper.cpp

Three runtimes serve the newer ASR models; only one of them speaks the executor's
`OPEN_AI_AUDIO` (`POST /v1/audio/transcriptions`, multipart) as it is:

- **nemo-speech** (`nemo-speech serve`, NVIDIA NeMo-Speech.cpp on ggml) answers
  `/v1/audio/transcriptions` and lists its model on `/v1/models`. The executor registers it
  directly.
- **sherpa-onnx** serves recognition only over a WebSocket protocol of its own
  (`sherpa-onnx-offline-websocket-server`: one binary message of sample rate, byte count and
  float32 samples; a JSON answer). It writes `log.txt` into its working directory, so the
  run starts it in the run's log directory.
- **llama-server** has `/v1/audio/transcriptions` in b11669, but it answers `json` only,
  sends the audio with no instruction and returns the model's raw text: Qwen3-ASR's begins
  `language English<asr_text>`, and Voxtral Mini, given audio alone, answers what is said
  ("I'm sorry, but I can't move meetings") instead of transcribing it.

For sherpa-onnx and llama-server the run starts an OpenAI-shaped front beside the server
(`providers/audio-front.ts`): it takes the executor's multipart upload, makes the server's
request (sherpa-onnx's WebSocket frame; llama-server's `/v1/chat/completions` with an
`input_audio` part) and answers `json`, `verbose_json` or `text`. For Qwen3-ASR it strips
the language prefix (and reports the language); for Voxtral it adds a system message that it
only transcribes and the instruction after the audio, without which it either answers the
speech or prefixes "Here's the transcript:". Even so it answers the human recording with
`[00:00:00]`, `[No transcript available]` or a refusal, depending on the wording: through
chat it is prompt-fragile, and its own transcription mode (`lang:xx[TRANSCRIBE]`) is not
reachable through llama-server. The cells measure server and front together: what an
executor endpoint kind or a sidecar would cost.

### Thinking models

Qwen3.5 thinks by default. The cells turn it off where it can be turned off, because an
app's `ai.prompt` has no way to (it sends no `max_tokens` and no thinking switch):

- llama-server is started with `--reasoning off` for LLMs. Without it, `ai.prompt` on
  Qwen3.5 thinks until the 8k context is spent and returns an empty answer.
- Ollama requests send `reasoning_effort: "none"`: the executor passes `/v1` chat through to
  Ollama's OpenAI route, where the reasoning otherwise fills `max_tokens` and no content
  comes. Ollama's `/v1/completions` applies the chat template and thinks whatever the
  request says (only its native raw `/api/generate` continues text), so the Ollama completion
  cell fails on a thinking model. That is the provider's behaviour, recorded as found.
- vLLM and llama-server requests carry `chat_template_kwargs: {enable_thinking: false}`.

The flags each server ran with are in the cell's `serverFlags` and in its recommendation.

### llama-server's ROCm build needs the system ROCm

The manifest's `linux-x64-rocm` build links the system ROCm runtime (`libamdhip64`,
rocBLAS, hipBLAS) rather than shipping it. Without those libraries on the search path it
finds no device and runs on the CPU without saying so. The run puts `/opt/rocm/lib` and
`/opt/rocm/core-*/lib` on its `LD_LIBRARY_PATH` (`AI_WT_ROCM_LIB` overrides), and every GPU
cell checks that the server held at least half the model in GPU memory, so a CPU fallback
fails rather than passing for the GPU. A local setup that installs the ROCm build must check
for a ROCm runtime the same way.

### vLLM on AMD

vLLM has no official image for consumer or APU AMD GPUs. On Strix Halo (gfx1151) the
community image `kyuz0/vllm-therock-gfx1151` (ROCm 10, vLLM 0.33) works:
`AI_WT_VLLM_ROCM_IMAGE=kyuz0/vllm-therock-gfx1151:rocm10.0.0-torch2.11.0-vllm0.33.0`. Its
first start compiles kernels for several minutes (`serverReadyMs`). Without the variable the
vLLM cells are skipped on AMD.

## Adding evidence and regenerating recommendations

1. Run the wind tunnel on the machine. It writes
   `ai-wind-tunnel/evidence/<class>/<date>-<fingerprint>.json` (a second run on the same day
   and hardware replaces the first; `--out` writes elsewhere).
2. `pnpm run ai-wind-tunnel-recommend` regenerates `recommendations.json` from every evidence
   file. `-- --check` fails when the checked-in file is stale (the unit tests check this too).
3. Commit both. Evidence files are reviewable: no host names, users or paths, only hardware.

How a recommendation is chosen (`SERVICE_RULES` in `recommend.ts`, also written into
`recommendations.json`): per class and service, the cells that passed and fit the machine's
memory are candidates; a candidate's metrics are the median of its medians across evidence
files; those over the service's floor (LLM: ≥ 15 decode tokens/s and ≤ 2 s to first token;
embeddings: ≥ 10 texts/s; STT: real-time factor ≤ 0.5; TTS: ≤ 1) are ranked (LLM: the most
parameters, then decode rate; STT: the mean WER over the clips in 0.02 buckets, then
real-time factor; TTS: round-trip
WER, then real-time factor; embeddings: throughput). The first wins and cites every evidence
file that measured it. A class with no evidence, or no candidate over the floor, has no
recommendation; the executor's local setup then falls back to the nearest smaller measured
class of the same family, then to the CPU classes (`fallbackClasses` in `hardware-class.ts`).

## Not in CI

The wind tunnel needs real models and minutes to hours of compute, so no PR job runs it. The
unit tests (`pnpm run test-ai-wind-tunnel-unit`) are cheap and cover the matrix, scoring,
statistics, classification, evidence validation and recommendation derivation. A nightly job
on a machine with models could run `pnpm run ai-wind-tunnel -- --repeat 5` and open a PR
with the new evidence; there is none yet.
