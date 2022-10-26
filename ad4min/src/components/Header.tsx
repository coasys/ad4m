import { Title, Image } from '@mantine/core';

type Props = {}

const Header = (props: Props) => {
  return (
    <>
      <Title order={1}>
        <Image style={{width: '200px'}} src="ad4msquarelogo2_white_colouremblem.png"></Image>
      </Title>
    </>
  )
}

export default Header