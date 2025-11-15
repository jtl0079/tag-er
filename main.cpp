#define SDL_MAIN_HANDLED
#define NOMINMAX
#include <iostream>
#include <vector>
#include <mutex>
#include <atomic>
#include <thread>
#include <Windows.h>

extern "C" {
#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libswresample/swresample.h>
#include <libavutil/opt.h>
}

#include <SDL.h>
using namespace std;




struct AudioBuffer {
	std::vector<uint8_t> data;
	std::mutex mtx;
};

AudioBuffer audioBuffer;
std::atomic<bool> quit(false);

void SDLAudioCallback(void* userdata, Uint8* stream, int len) {
	std::lock_guard<std::mutex> lock(audioBuffer.mtx);
	
	if (audioBuffer.data.empty()) {
		SDL_memset(stream, 0, len);
		return;
	}

	int copyLen = std::min(len, static_cast<int>(audioBuffer.data.size()));
	SDL_memcpy(stream, audioBuffer.data.data(), copyLen);
	audioBuffer.data.erase(audioBuffer.data.begin(), audioBuffer.data.begin() + copyLen);

	if (copyLen < len) SDL_memset(stream + copyLen, 0, len - copyLen);
}

bool loadingPath(AVFormatContext*& fmtCtx) {

	const char* filename = "C:\\Cpp_Files\\MyCategory\\x64\\Debug\\1.flac";

	// 读取文件的来源和类型
	if (avformat_open_input(&fmtCtx, filename, nullptr, nullptr) < 0) {
		cout << "can't open file " << filename << "\n"; return false;
	}

	// 读取全部流的信息
	if (avformat_find_stream_info(fmtCtx, nullptr) < 0) {
		cout << "can't find stream info. " << filename << "\n"; return false;
	}


	return true;
}

bool decoding(AVStream* stream, const AVCodec*& codec, AVCodecContext* codecCtx) {
	// AVStream			数据源
	// AVCodec			解码器
	// AVCodecContext	解码工具间
	// 


	// 在工具间里放入需要的资料
	if (avcodec_parameters_to_context(codecCtx, stream->codecpar) < 0) {
		cout << "Failed to copy codec parameters to context \n"; return false;
	}

	// 在工具间里初始化解码实例
	if (avcodec_open2(codecCtx, codec, nullptr) < 0) {
		cout << "Failed to open decoder\n"; return false;
	}

	return true;
}

bool resampleInit(SwrContext*& swrCtx, AVCodecContext*& codecCtx, int out_sample_rate = 44100) {
	// 磨合输出要求和源数据配置
	int ret = swr_alloc_set_opts2(
		&swrCtx,
		&codecCtx->ch_layout,
		AV_SAMPLE_FMT_S16,
		out_sample_rate,
		&codecCtx->ch_layout,
		codecCtx->sample_fmt,
		codecCtx->sample_rate,
		0,
		nullptr
	);

	if (ret < 0) {
		cout << "swr_alloc_set_opts2 failed \n";	return false;
	}

	// 磨合成功后进行初始化
	if (swr_init(swrCtx) < 0) {
		cout << "swr_init failed\n";	
		swr_free(&swrCtx);
		return false;
	}

	return true;
}

int main(int argc, char* argv[]) {
	SetConsoleOutputCP(CP_UTF8);
	//SetConsoleCP(CP_UTF8);
	if (SDL_Init(SDL_INIT_AUDIO) < 0) {
		cout << "error\n";
	}
	
	const char8_t* filename = u8"C:\\Cpp_Files\\MyCategory\\x64\\Debug\\1.flac我的";

	av_log_set_level(AV_LOG_ERROR);
	avformat_network_init();

	AVFormatContext* fmtCtx = nullptr;
	
	loadingPath(fmtCtx);
	
	for (unsigned i = 0; i < fmtCtx->nb_streams; i++) {
		cout << av_get_media_type_string(fmtCtx->streams[i]->codecpar->codec_type) << "\t" << "\n";
	}




	int audioStreamIndex = av_find_best_stream(fmtCtx, AVMEDIA_TYPE_AUDIO, -1, -1, nullptr, 0);
	if (audioStreamIndex < 0) { avformat_close_input(&fmtCtx); return -1; }

	AVStream* audioStream = fmtCtx->streams[audioStreamIndex];
	
	// 根据流的类型匹配适合的解码器
	const AVCodec* codec = avcodec_find_decoder(audioStream->codecpar->codec_id);
	if (!codec) {
		avformat_close_input(&fmtCtx);
		return -1;
	}

	// 根据解码器类型匹配要解码的工具间
	AVCodecContext* codecCtx = avcodec_alloc_context3(codec);
	if (!codecCtx) {
		avformat_close_input(&fmtCtx);
		return -1;
	}
	

	decoding(fmtCtx->streams[0], codec, codecCtx);
	cout << codecCtx->sample_rate << "\n";

	// --- swr_alloc_set_opts2 is the modern API; do not use swr_alloc_set_opts (deprecated) ---
	SwrContext* swrCtx = nullptr;
	resampleInit(swrCtx, codecCtx); 

	if (SDL_Init(SDL_INIT_AUDIO) < 0) {
		swr_free(&swrCtx);
		avcodec_free_context(&codecCtx);
		avformat_close_input(&fmtCtx);
		return -1;
	}

	//播放设备参数
	SDL_AudioSpec spec;
	spec.freq = 44100;
	spec.format = AUDIO_S16SYS;
	spec.channels = 2;
	spec.samples = 1024;
	spec.callback = SDLAudioCallback;
	spec.userdata = nullptr;
	

	if (SDL_OpenAudio(&spec, nullptr) < 0) {
		SDL_Quit();
		swr_free(&swrCtx);
		avcodec_free_context(&codecCtx);
		avformat_close_input(&fmtCtx);
		return -1;
	}

	SDL_PauseAudio(0);

	AVPacket* packet = av_packet_alloc();
	AVFrame* frame = av_frame_alloc();
	std::vector<uint8_t> outBuffer(192000); // big enough for converted data
	

	while (av_read_frame(fmtCtx, packet) >= 0 && !quit) {
		if (packet->stream_index == audioStreamIndex) {
			if (avcodec_send_packet(codecCtx, packet) == 0) {
				while (avcodec_receive_frame(codecCtx, frame) == 0) {
					// convert
					uint8_t* outArr[1] = { outBuffer.data() };
					int outSamples = swr_convert(
						swrCtx,
						outArr,
						frame->nb_samples,
						(const uint8_t**)frame->extended_data,
						frame->nb_samples
					);


					if (outSamples > 0) {
						int outDataSize = av_samples_get_buffer_size(
							nullptr,
							spec.channels,
							outSamples,
							AV_SAMPLE_FMT_S16,
							1
						);

						std::lock_guard<std::mutex> lock(audioBuffer.mtx);
						audioBuffer.data.insert(audioBuffer.data.end(), outBuffer.data(), outBuffer.data() + outDataSize);
					}
				}
			}
		}
		
		av_packet_unref(packet);
	}
	
	// drain decoder
	avcodec_send_packet(codecCtx, nullptr);
	while (avcodec_receive_frame(codecCtx, frame) == 0) {
		uint8_t* outArr[1] = { outBuffer.data() };
		int outSamples = swr_convert(
			swrCtx,
			outArr,
			frame->nb_samples,
			(const uint8_t**)frame->extended_data,
			frame->nb_samples
		);

		if (outSamples > 0) {
			int outDataSize = av_samples_get_buffer_size(
				nullptr,
				spec.channels,
				outSamples,
				AV_SAMPLE_FMT_S16,
				1
			);
			std::lock_guard<std::mutex> lock(audioBuffer.mtx);
			audioBuffer.data.insert(audioBuffer.data.end(), outBuffer.data(), outBuffer.data() + outDataSize);
		}
	}
	
	// wait for buffer to finish
	while (true) {
		{
			std::lock_guard<std::mutex> lock(audioBuffer.mtx);
			if (audioBuffer.data.empty()) break;
		}
		SDL_Delay(100);

	}

	quit = true;
	SDL_CloseAudio();
	SDL_Quit();

	swr_free(&swrCtx);
	avcodec_free_context(&codecCtx);
	avformat_close_input(&fmtCtx);
	av_frame_free(&frame);
	av_packet_free(&packet);

	return 0;
}





SDL_Window* q() {
	SDL_Window* win = SDL_CreateWindow(
		"title",
		SDL_WINDOWPOS_CENTERED,
		SDL_WINDOWPOS_CENTERED,
		100, 100,
		SDL_WINDOW_SHOWN
	);
	return win;
}
