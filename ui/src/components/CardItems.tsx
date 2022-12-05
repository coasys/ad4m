import { Text, Space, createStyles } from '@mantine/core';

type Props = {
  title: string;
  value?: string;
  titleUnderline?: boolean;
}

const useStyles = createStyles((_theme, _params, getRef) => {
  return {
    row: {
      display: 'flex', 
      alignItems: 'self-start'
    },
    textStyle: {
      fontWeight: 300, 
      wordBreak: 'break-all'
    }
  };
});

const CardItems = (props: Props) => {
  const { classes, cx } = useStyles();

  return (
    <div className={cx(classes.row)} >
      <Text weight="bold" style={{minWidth: 90}} underline={props.titleUnderline}>{props.title}: </Text>
      <Space w="sm" />
      <Text className={cx(classes.textStyle)} size="sm">{props.value}</Text>
    </div>
  )
}

export default CardItems