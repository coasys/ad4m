type ActionButtonProps = {
  iconColor?: string;
  title: string;
  onClick: () => void;
  icon: string;
};

export default function ActionButton({
  title,
  iconColor,
  onClick,
  icon,
}: ActionButtonProps) {
  return (
    <div >
      <j-tooltip title={title} placement="bottom">
        <j-button onClick={onClick} square circle size="xl" variant="subtle">
          <j-icon color={iconColor} size="md" name={icon}></j-icon>
        </j-button>
      </j-tooltip>
    </div>
  );
}
