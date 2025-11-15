#include "Tag.h"

namespace Conversion {

	std::string toUTF8String(const char* s) {
		return s ? std::string(s) : std::string();
	}

	std::string toUTF8String(const std::string& s) {
		return s;
	}

	std::string toUTF8String(const char8_t* s) {
		return s ? std::string(reinterpret_cast<const char*>(s)) : std::string();
	}

	std::string toUTF8String(const std::u8string& s) {
		return std::string(reinterpret_cast<const char*>(s.data()), s.size());
	}

}

namespace FFmpegTool {

	// 使用 stream 和它的 fmtCtx 检测
	// 返回 stream 的时长
	double getStreamDurationSec(const AVFormatContext* fmtCtx, const AVStream* stream) {
		if (!stream || !fmtCtx)
			return 0.0;

		double durationSec = 0.0;

		// ① 优先使用流自己的 duration
		if (stream->duration != AV_NOPTS_VALUE && stream->duration > 0) {
			durationSec = stream->duration * av_q2d(stream->time_base);
		}

		// ② 尝试根据 nb_frames / avg_frame_rate 推算
		else if (stream->nb_frames > 0 && stream->avg_frame_rate.num > 0 && stream->avg_frame_rate.den > 0) {
			durationSec = (double)stream->nb_frames / av_q2d(stream->avg_frame_rate);
		}

		// ③ 再尝试容器级 duration
		else if (fmtCtx->duration != AV_NOPTS_VALUE && fmtCtx->duration > 0) {
			durationSec = fmtCtx->duration / (double)AV_TIME_BASE;
		}

		// ④ 最后使用 start_time + estimated_duration 逻辑
		else if (stream->start_time != AV_NOPTS_VALUE) {
			// 有些格式 duration 缺失，但 start_time 存在，可以推测
			durationSec = (fmtCtx->duration != AV_NOPTS_VALUE)
				? (fmtCtx->duration / (double)AV_TIME_BASE)
				: (stream->start_time * av_q2d(stream->time_base));
		}

		if (durationSec <= 0.0 && fmtCtx->duration > 0)
			durationSec = fmtCtx->duration / (double)AV_TIME_BASE;

		// ⑤ 确保非负
		if (durationSec < 0.0 || std::isnan(durationSec))
			durationSec = 0.0;

		return durationSec;
	}

	// 获取音频通道信息（兼容 FFmpeg 5~8）
	std::string getChannelDescription(const AVCodecParameters* codecpar) {
		// FFmpeg 8+ 使用 AVChannelLayout
#if LIBAVUTIL_VERSION_MAJOR >= 58
		const AVChannelLayout* layout = &codecpar->ch_layout;
		if (layout && layout->nb_channels > 0) {
			char buf[128] = { 0 };
			if (av_channel_layout_describe(layout, buf, sizeof(buf)) >= 0)
				return std::string(buf);
			else
				return std::to_string(layout->nb_channels) + " ch";
		}
#else
	// 老版本使用 channels + channel_layout
		if (codecpar->channels > 0) {
			char buf[128] = { 0 };
			av_get_channel_layout_string(buf, sizeof(buf),
				codecpar->channels,
				codecpar->channel_layout);
			return std::string(buf);
		}
#endif
		return "Unknown";
	}

	bool AVFormatContext_open(AVFormatContext*& fmtCtx, const char* path) {
		// 如果之前有内容就先释放
		if (fmtCtx) {
			avformat_close_input(&fmtCtx);
			fmtCtx = nullptr;
		}

		// 读取文件的来源和类型
		if (avformat_open_input(&fmtCtx, path, nullptr, nullptr) < 0) {
			std::cout << "can't open file " << path << "\n"; return false;
		}

		// 读取全部流的信息
		if (avformat_find_stream_info(fmtCtx, nullptr) < 0) {
			std::cout << "can't find stream info. " << path << "\n"; return false;
		}
		return true;
	}

	void AVFormatContext_close(AVFormatContext*& fmtCtx) {
		if (fmtCtx) {
			avformat_close_input(&fmtCtx);
			fmtCtx = nullptr;
		}
	}

	int printMediaAllStreamDetail(const char* path) {
		// Open input file
		AVFormatContext* fmtCtx = nullptr;
		if (avformat_open_input(&fmtCtx, path, nullptr, nullptr) < 0) {
			std::cerr << "Failed to open file: " << path << std::endl;
			return -1;
		}


		// Retrieve stream information
		if (avformat_find_stream_info(fmtCtx, nullptr) < 0) {
			std::cerr << "Failed to retrieve stream info" << std::endl;
			avformat_close_input(&fmtCtx);
			return -1;
		}

		std::cout << "Opened successfully! Total streams: " << fmtCtx->nb_streams << "\n";

		// Iterate over all streams
		for (unsigned int i = 0; i < fmtCtx->nb_streams; ++i) {
			AVStream* stream = fmtCtx->streams[i];
			AVCodecParameters* codecpar = stream->codecpar;

			std::cout << "----------------------------------------------------\n";
			std::cout << "Stream #" << i << " | Type: ";

			switch (codecpar->codec_type) {
			case AVMEDIA_TYPE_VIDEO:
				std::cout << "Video\n";
				std::cout << "  Codec ID  : " << codecpar->codec_id << "\n";
				std::cout << "  Resolution: " << codecpar->width << "x" << codecpar->height << "\n";
				std::cout << "  FPS (avg) : " << av_q2d(stream->avg_frame_rate) << "\n";
				break;

			case AVMEDIA_TYPE_AUDIO:
				std::cout << "Audio\n";
				std::cout << "  Codec ID   : " << codecpar->codec_id << "\n";
				std::cout << "  Sample rate: " << codecpar->sample_rate << " Hz\n";
				std::cout << "  Channels   : " << getChannelDescription(codecpar) << "\n";
				std::cout << "  Bitrate    : " << codecpar->bit_rate << " bps\n";
				break;

			case AVMEDIA_TYPE_SUBTITLE:
				std::cout << "Subtitle\n";
				break;

			case AVMEDIA_TYPE_DATA:
				std::cout << "Data\n";
				break;

			case AVMEDIA_TYPE_ATTACHMENT:
				std::cout << "Attachment\n";
				break;

			default:
				std::cout << "Unknown\n";
				break;
			}

			double durationSec = stream->duration * av_q2d(stream->time_base);
			std::cout << "  Duration : " << durationSec << " sec\n";
			std::cout << "  Time base: " << stream->time_base.num << "/" << stream->time_base.den << "\n";
		}

		std::cout << "----------------------------------------------------\n";
		avformat_close_input(&fmtCtx);
		avformat_free_context(fmtCtx);
		return 0;
	}
}


std::vector<size_t> PrintTableTool::calculateColumnsWidth(const std::vector<std::vector<std::string>>& data) {
	if (data.empty()) return {};
	vector<size_t> widths(data[0].size(), 0);
	for (const auto& row : data) {
		for (size_t i = 0; i < row.size(); ++i) {
			widths[i] = max(widths[i], row[i].size());
		}
	}
	return widths;
}

void PrintTableTool::printSeparator(const vector<size_t>& widths, char horizontal, char junction) {
	cout << junction;
	for (size_t w : widths) {
		cout << string(w + 2, horizontal) << junction;
	}
	cout << "\n";
}

void PrintTableTool::printRow(const vector<string>& row, const vector<size_t>& widths, char vertical) {
	cout << vertical;
	for (size_t i = 0; i < row.size(); ++i) {
		cout << " " << setw(widths[i]) << left << row[i] << " " << vertical;
	}
	cout << "\n";

}


// 根据ID获取字符串，返回 optional
std::optional<std::reference_wrapper<const std::string>> NameRegistry::getString(long long id) const {
	auto it = idToStr.find(id);
	if (it != idToStr.end()) return it->second;
	return std::nullopt;
}

// 根据字符串获取ID，返回 optional
std::optional<long long> NameRegistry::getId(const std::string& name) const {
	auto it = strToId.find(name);
	if (it != strToId.end()) return it->second;
	return std::nullopt;
}