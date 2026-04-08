interface Props {
  ok: boolean;
  label: string;
}

export function StatusBadge({ ok, label }: Props) {
  return (
    <span class={`status-badge ${ok ? 'status-ok' : 'status-err'}`}>
      {ok ? '●' : '○'} {label}
    </span>
  );
}
