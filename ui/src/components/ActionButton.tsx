type ActionButtonProps = {
  tooltip: string;
  onClick: () => void;
  icon: string;
}

export default function ActionButton({tooltip, onClick, icon}: ActionButtonProps) {
  return (
    <j-tooltip title={tooltip} placement="bottom">
      <j-button
        onClick={onClick}
        square
        circle
        size="xl"
        variant="subtle"
      >
        <j-icon size="md" name={icon}></j-icon>
      </j-button>
    </j-tooltip>
  )
}
