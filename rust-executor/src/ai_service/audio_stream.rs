use std::pin::Pin;

use futures::StreamExt;
//use kalosm::sound::AsyncSource;
use tokio_stream::Stream;

pub struct AudioStream {
    //pub drop_tx: std::sync::mpsc::Sender<()>,
    pub read_data: Vec<f32>,
    pub receiver: Pin<Box<dyn futures_core::Stream<Item = f32> + Send + Sync>>,
}

//impl Drop for AudioStream {
//    fn drop(&mut self) {
//        self.drop_tx.send(()).unwrap();
//    }
//}

impl Stream for AudioStream {
    type Item = f32;

    fn poll_next(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        match self.receiver.as_mut().poll_next_unpin(cx) {
            std::task::Poll::Ready(Some(data_chunk)) => {
                self.read_data.push(data_chunk);
                std::task::Poll::Ready(Some(data_chunk))
            }
            std::task::Poll::Ready(None) => std::task::Poll::Ready(None),
            std::task::Poll::Pending => std::task::Poll::Pending,
        }
    }
}

// impl AudioStream {
//     /// Read any pending data from the stream into a vector
//     fn read_sync(&mut self) -> Vec<f32> {
//         let mut cx = std::task::Context::from_waker(futures_util::task::noop_waker_ref());
//         while let std::task::Poll::Ready(Some(data_chunk)) = self.receiver.poll_next_unpin(&mut cx)
//         {
//             self.read_data.push(data_chunk);
//         }
//         self.read_data.clone()
//     }

//     /// Grab all current data in the stream
//     fn read_all_samples(&mut self) -> Vec<f32> {
//         self.read_sync()
//     }

//     fn read_all(&mut self) -> SamplesBuffer<f32> {
//         let channels = 1;
//         let sample_rate = 160000;
//         SamplesBuffer::new(channels, sample_rate, self.read_all_samples())
//     }
// }

// impl AsyncSource for AudioStream {
//     fn as_stream(&mut self) -> impl Stream<Item = f32> + '_ {
//         self
//     }

//     fn sample_rate(&self) -> u32 {
//         160000
//     }
// }
