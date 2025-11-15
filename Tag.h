#pragma once
#define SDL_MAIN_HANDLED
#define NOMINMAX
#include <cctype>
#include <filesystem>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <mutex>
#include <cmath>
#include <optional>
#include <queue>
#include <set>
#include <shared_mutex>
#include <string>
#include<unordered_map>
#include <unordered_set>
#include <vector>


extern "C" {
#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libswresample/swresample.h>
#include <libavutil/samplefmt.h>
#include <libavutil/opt.h>
}

#include <SDL.h>

/*				| 内存占用	| 尾插/尾删	| 头插/头删
 * vector		| 最少		| O(1)		| O(n)
 * deque		| 很少		| O(1)		| O(1)
 * list			| 多			| O(1)		| O(1)
 * map			| 很多		| 			|
 * unordered_map| 最多		|			|
 */


namespace PrintTableTool {
	using namespace std;

	vector<size_t> calculateColumnsWidth(const vector<vector<string>>& data);

	void printSeparator(const vector<size_t>& widths, char horizontal, char junction);

	void printRow(const vector<string>& row, const vector<size_t>& widths, char vertical);
}

namespace Validation {
	template<typename... Args>
	bool all_of(Args... args) {
		return (args && ...);
	}

	template<typename T>
	bool all_of(std::initializer_list<T> list) {
		return std::all_of(list.begin(), list.end(), [](const T& x) { return x; });
	}

	template<typename T, std::size_t N>
	bool all_of(const T(&arr)[N]) {
		return std::all_of(std::begin(arr), std::end(arr), [](const T& x) { return x; });
	}

	template<typename... Args>
	bool any_of(Args... args) {
		return (args || ...); // fold expression
	}

	template<typename T>	// 给
	bool any_of(std::initializer_list<T> list) {
		return std::any_of(list.begin(), list.end(), [](const T& x) { return x; });
	}

	template<typename T, std::size_t N>
	bool any_of(const T(&arr)[N]) {
		return std::any_of(std::begin(arr), std::end(arr), [](const T& x) { return x; });
	}


	constexpr bool anyDigit(std::string_view str) noexcept {
		for (unsigned char ch : str) {
			if (ch >= '0' && ch <= '9')
				return true;
		}
		return false;
	}

	// 是否包含英文字母 (A-Z, a-z)
	constexpr bool anyAlpha(std::string_view str) noexcept {
		for (unsigned char ch : str)
			if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z'))
				return true;
		return false;
	}

	// 是否包含大写字母
	constexpr bool anyUpper(std::string_view str) noexcept {
		for (unsigned char ch : str)
			if (ch >= 'A' && ch <= 'Z')
				return true;
		return false;
	}

	// 是否包含小写字母
	constexpr bool anyLower(std::string_view str) noexcept {
		for (unsigned char ch : str)
			if (ch >= 'a' && ch <= 'z')
				return true;
		return false;
	}

	// 是否包含空白字符（空格、tab、换行）
	constexpr bool anySpace(std::string_view str) noexcept {
		for (unsigned char ch : str)
			if (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r')
				return true;
		return false;
	}

	// 是否包含符号字符（非字母、非数字）
	constexpr bool anySymbol(std::string_view str) noexcept {
		for (unsigned char ch : str)
			if (!((ch >= '0' && ch <= '9') ||
				(ch >= 'A' && ch <= 'Z') ||
				(ch >= 'a' && ch <= 'z') ||
				ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'))
				return true;
		return false;
	}

	// 用来检测「标点符号字符」
	constexpr bool anyPunct(std::string_view str) noexcept {
		for (unsigned char ch : str)
			if ((ch >= 33 && ch <= 47) ||  // !"#$%&'()*+,-./
				(ch >= 58 && ch <= 64) ||  // :;<=>?@
				(ch >= 91 && ch <= 96) ||  // [\]^_`
				(ch >= 123 && ch <= 126))  // {|}~
				return true;
		return false;
	}

}

namespace Conversion {
	std::string toUTF8String(const char* s);
	std::string toUTF8String(const std::string& s);
	std::string toUTF8String(const char8_t* s);
	std::string toUTF8String(const std::u8string& s);
}

//* NameRegistry 将名字转换为 long 使运行时查找速度提升
// 因为使用 str 查文件时 O(k)，其中 k = 字符串长度。而 long 是 O(1)
// 这样就把「重复的字符串操作」变成「一次性字符串操作 + 高速整数操作」
// 
class NameRegistry {
public:
	std::unordered_map<std::string, long long> strToId;
	std::unordered_map<long long, std::string> idToStr;
	long long nextId = 0;

	// 添加字符串节点，返回分配的ID，如果已存在返回已有ID
	long long add(const std::string& name) {
		auto it = strToId.find(name);
		if (it != strToId.end()) return it->second;

		long long id = nextId++;
		strToId.emplace(name, id);
		idToStr.emplace(id, name);
		return id;
	}

	// 根据ID获取字符串，返回 optional
	std::optional<std::reference_wrapper<const std::string>> getString(long long id) const;

	// 根据字符串获取ID，返回 optional
	std::optional<long long> getId(const std::string& name) const;

	// 删除字符串及对应ID
	bool remove(long long id) {
		auto it = idToStr.find(id);
		if (it == idToStr.end()) return false;

		const std::string& name = it->second;
		strToId.erase(name);
		idToStr.erase(it);
		return true;
	}
};




// Node 负责上下级关系，eg. 在文件夹你知道下一步和上一步
// Tree 负责所有层级关系，eg. 在文件管理器你知道设备的所有文件信息和统计
// Forest 负责分开每个 Tree，eg. 你可以区分不同设备
// 
class Node {
public:
	Node* parent;
	std::unordered_set<Node*> children;

	// 默认建造一个无根无子集的节点
	Node(Node* p = nullptr) : parent(p) {}

	// 禁用拷贝和移动，确保指针唯一性
	Node(const Node&) = delete;
	Node& operator=(const Node&) = delete;
	Node(Node&&) = delete;
	Node& operator=(Node&&) = delete;

	// 安全析构：只清理 children 关系，不删除指针
	virtual ~Node() {
		// 从父节点的 children 中移除自己
		if (parent) {
			parent->children.erase(this);
		}

		// 断开与子节点的关系（不删除子节点）
		for (auto child : children) {
			if (child) {
				child->parent = nullptr;
			}
		}
		children.clear();
	}


	// 添加子节点 - 增加循环检测
	bool addChild(Node* child) {
		if (!child || child == this) {
			return false;
		}

		// 检测循环引用
		if (isAncestorOf(child)) {
			return false;
		}

		// 如果 child 已有父节点，先移除其父节点的自己
		if (child->parent) {
			child->parent->removeChild(child);

			auto it = child->parent->children.find(child);
			if (it != child->parent->children.end()) {
				child->parent->children.erase(it);
			}
		}

		children.emplace(child);	// 写入自己的 children
		child->parent = this;		// 把 child 的父节点改成自己
		return true;
	}

	// 移除子节点
	bool removeChild(Node* child) {
		if (!child) return false;	// child 不是 null

		auto it = children.find(child);
		if (it != children.end()) {
			children.erase(it);
			child->parent = nullptr;
			return true;
		}
		return false;
	}

	// 实用方法
	bool isRoot() const { return parent == nullptr; }
	bool isLeaf() const { return children.empty(); }
	size_t getChildCount() const { return children.size(); }

	int getDepth() const {
		int depth = 0;
		Node* current = parent;
		while (current) {
			depth++;
			current = current->parent;
		}
		return depth;
	}

	// 检查是否是某个节点的祖先（防止循环）
	bool isAncestorOf(const Node* node) const {
		if (!node) return false;
		const Node* current = node->parent;
		while (current) {
			if (current == this) return true;
			current = current->parent;
		}
		return false;
	}

	// 获取根节点
	Node* getRoot() {
		Node* current = this;
		while (current->parent) {
			current = current->parent;
		}
		return current;
	}

	const Node* getRoot() const {
		const Node* current = this;
		while (current->parent) {
			current = current->parent;
		}
		return current;
	}

	// 获取所有祖先节点（从父节点到根节点）
	std::vector<Node*> getAncestors() const {
		std::vector<Node*> ancestors;
		const Node* current = parent;
		while (current) {
			ancestors.push_back(const_cast<Node*>(current));
			current = current->parent;
		}
		return ancestors;
	}

	// 获取所有后代节点
	std::vector<Node*> getDescendants() const {
		std::vector<Node*> descendants;
		std::function<void(const Node*)> collect = [&](const Node* node) {
			for (Node* child : node->children) {
				descendants.push_back(child);
				collect(child);
			}
			};
		collect(this);
		return descendants;
	}

	// 查找最近公共祖先
	Node* findCommonAncestor(Node* other) const {
		if (!other) return nullptr;

		auto thisAncestors = getAncestors();
		auto otherAncestors = other->getAncestors();

		// 将this加入祖先列表
		thisAncestors.insert(thisAncestors.begin(), const_cast<Node*>(this));
		otherAncestors.insert(otherAncestors.begin(), other);

		std::unordered_set<Node*> thisSet(thisAncestors.begin(), thisAncestors.end());

		for (Node* ancestor : otherAncestors) {
			if (thisSet.find(ancestor) != thisSet.end()) {
				return ancestor;
			}
		}
		return nullptr;
	}
};

// Tree 负责所有层级关系
class Tree {
public:
	// 生命周期管理（唯一所有权）
	std::unordered_map<Node*, std::unique_ptr<Node>> nodes;	// Nodes 是所有节点的集，方便从任意节点开始搜寻
	Node* root = nullptr;

	virtual ~Tree() = default;

	// 禁用拷贝，允许移动
	Tree(const Tree&) = delete;
	Tree& operator=(const Tree&) = delete;
	Tree(Tree&&) = default;
	Tree& operator=(Tree&&) = default;

	Tree() = default;

	// 添加一个节点
	Node* addNode(Node* parent = nullptr) {
		// 1. 创建一个空的 Node
		// 2. 创建这个 Node 的 unique_ptr, 并将 Node 的指针和 unique_ptr 放入 nodes
		// 3. 将 parent 放入这个 Node 的 parent 并在这个 parent 的 children 写入这个 Node （如果没有 parent 则不需要）
		//

		/* 验证父节点存在于此树中
		if (parent && !contains(parent)) {
			throw std::invalid_argument("Parent node does not belong to this tree");
		}
		*/

		// 创建 Node，指定父节点
		auto nodePtr = std::make_unique<Node>(parent);
		Node* rawNodePtr = nodePtr.get(); // 保留裸指针，用于 parent/children 和索引


		// 生命周期管理：放入 node map
		nodes.emplace(rawNodePtr, std::move(nodePtr));

		// 建立父子关系
		if (parent) {
			parent->children.insert(rawNodePtr);
		}
		else if (!root) {
			root = rawNodePtr;
		}

		// 钩子方法，供衍生类重写
		onNodeAdded(rawNodePtr);

		return rawNodePtr; // 返回裸指针，外部可以使用
	}

	// 移除该节点，它的子节点将绑定它的父节点
	bool removeNode(Node* node) {
		if (!node || nodes.find(node) == nodes.end()) {
			return false;
		}

		// 收集要被移除的节点的子节点
		std::vector<Node*> childrenCopy(node->children.begin(), node->children.end());

		// 处理根节点删除的情况
		if (root == node) {
			if (childrenCopy.empty()) {	// 当被删除的是根且没有小孩，表示这个 tree 已经没有东西可以延续（可以有但是需要手动定义新的 root）
				root = nullptr;
			}
			else if (childrenCopy.size() == 1) {
				root = childrenCopy[0];
				root->parent = nullptr;
			}
			else {
				// 多个子节点时，选择第一个作为新根，其他作为新根的子节点
				root = childrenCopy[0];
				root->parent = nullptr;
				for (size_t i = 1; i < childrenCopy.size(); ++i) {
					root->addChild(childrenCopy[i]);
				}
			}
		}
		else if (node->parent) {
			// 将所有子节点提升到祖父节点
			for (Node* child : childrenCopy) {
				node->parent->addChild(child);
			}
		}



		onNodeRemoved(node);

		// 移除节点（unique_ptr自动处理内存释放）
		nodes.erase(node);
		return true;
	}

	// 移动节点到新父节点下
	bool moveNode(Node* node, Node* newParent) {
		if (!node || !contains(node)) return false;			// 防止 node 本身不存在
		if (newParent && !contains(newParent)) return false;// 防止虽然 newParent 不是 null 但是它并不在书里面
		if (node == newParent) return false;				// 防止自己移动到自己
		if (node->isAncestorOf(newParent)) return false;	// 防止循环

		// 移除旧的父子关系 (去擦除 parent 里的 children 的自己)
		if (node->parent) {
			node->parent->children.erase(node);
		}

		// 建立新的父子关系
		if (newParent) {
			// 普通情况：挂到 newParent
			node->parent = newParent;
			newParent->children.emplace(node);
		}
		else {
			// 情况：要把 node 提升为新的 root
			if (root && root != node) {
				Node* oldRoot = root;

				// 旧 root 挂到新 root 下面
				node->addChild(oldRoot);
			}

			node->parent = nullptr;
			root = node;
		}

		return true;
	}

	// 查询方法
	bool contains(Node* node) const {
		return nodes.find(node) != nodes.end();
	}

	size_t size() const { return nodes.size(); }
	bool empty() const { return nodes.empty(); }
	Node* getRoot() const { return root; }

	// 获取指定深度的节点
	std::vector<Node*> getNodesAtDepth(int targetDepth) const {
		std::vector<Node*> result;
		if (root && targetDepth >= 0) {
			getNodesAtDepthHelper(root, 0, targetDepth, result);
		}
		return result;
	}

	// 获取树的最大深度
	int getMaxDepth() const {
		if (!root) return -1;
		return getMaxDepthHelper(root);
	}

	// 获取所有叶子节点
	std::vector<Node*> getLeafNodes() const {
		std::vector<Node*> leaves;
		for (const auto& [node, ptr] : nodes) {
			if (node->isLeaf()) {
				leaves.push_back(node);
			}
		}
		return leaves;
	}

	// 遍历方法

	template<typename Func>
	void traverseBFS(Node* startNode, Func func) const {
		if (!startNode) return;

		std::queue<Node*> queue;
		queue.push(startNode);

		while (!queue.empty()) {
			Node* current = queue.front();
			queue.pop();

			func(current);

			for (Node* child : current->children) {
				queue.push(child);
			}
		}
	}

	template<typename Func>
	void traversePreOrder(Node* startNode, Func func) const {
		if (!startNode) return;
		func(startNode);
		for (Node* child : startNode->children) {
			traversePreOrder(child, func);
		}
	}

	template<typename Func>
	void traversePostOrder(Node* startNode, Func func) const {
		if (!startNode) return;
		for (Node* child : startNode->children) {
			traversePostOrder(child, func);
		}
		func(startNode);
	}

	// 查找节点
	template<typename Predicate>
	Node* findNode(Predicate pred) const {
		for (const auto& [node, ptr] : nodes) {
			if (pred(node)) {
				return node;
			}
		}
		return nullptr;
	}

	// 查找所有匹配的节点
	template<typename Predicate>
	std::vector<Node*> findAllNodes(Predicate pred) const {
		std::vector<Node*> result;
		for (const auto& [node, ptr] : nodes) {
			if (pred(node)) {
				result.push_back(node);
			}
		}
		return result;
	}

	// 验证树的完整性
	bool validateTree() const {
		if (empty()) return true;
		if (!root) return false;

		// 检查根节点
		if (root->parent != nullptr) return false;
		if (!contains(root)) return false;

		// 检查所有节点
		for (const auto& [node, ptr] : nodes) {
			// 检查父子关系一致性
			if (node->parent) {
				if (node->parent->children.find(node) == node->parent->children.end()) {
					return false;
				}
				if (!contains(node->parent)) return false;
			}

			for (Node* child : node->children) {
				if (!child || child->parent != node || !contains(child)) {
					return false;
				}
			}
		}

		return true;
	}

	// 获取树的统计信息
	struct TreeStats {
		size_t totalNodes;
		size_t leafNodes;
		int maxDepth;
		double averageDepth;
	};

	TreeStats getStats() const {
		TreeStats stats{};
		stats.totalNodes = nodes.size();
		stats.maxDepth = getMaxDepth();

		if (empty()) {
			stats.averageDepth = 0.0;
			return stats;
		}

		int totalDepth = 0;
		for (const auto& [node, ptr] : nodes) {
			if (node->isLeaf()) {
				stats.leafNodes++;
			}
			totalDepth += node->getDepth();
		}

		stats.averageDepth = static_cast<double>(totalDepth) / stats.totalNodes;
		return stats;
	}


protected:
	// 钩子方法，供衍生类重写
	virtual void onNodeAdded(Node* node) {}
	virtual void onNodeRemoved(Node* node) {}

private:
	void getNodesAtDepthHelper(Node* node, int currentDepth, int targetDepth,
		std::vector<Node*>& result) const {
		if (currentDepth == targetDepth) {
			result.push_back(node);
			return;
		}

		for (Node* child : node->children) {
			getNodesAtDepthHelper(child, currentDepth + 1, targetDepth, result);
		}
	}

	int getMaxDepthHelper(Node* node) const {
		if (!node || node->children.empty()) {
			return 0;
		}

		int maxChildDepth = 0;
		for (Node* child : node->children) {
			maxChildDepth = std::max(maxChildDepth, getMaxDepthHelper(child));
		}
		return maxChildDepth + 1;
	}
};

// Forest 负责分开每个 Tree，eg. 你可以区分不同设备
class Forest {
public:
	std::unordered_map<Tree*, std::unique_ptr<Tree>> trees;

	Forest() = default;

	virtual ~Forest() = default;

	// 禁用拷贝，允许移动
	Forest(const Forest&) = delete;
	Forest& operator=(const Forest&) = delete;
	Forest(Forest&&) = default;
	Forest& operator=(Forest&&) = default;

	// 添加一个新 Tree，并返回它的指针
	Tree* addTree() {
		auto tree = std::make_unique<Tree>();
		Tree* rawPtr = tree.get();
		trees[rawPtr] = std::move(tree);
		return rawPtr;
	}

	// 添加现有的Tree（转移所有权）
	Tree* addTree(std::unique_ptr<Tree> tree) {
		if (!tree) return nullptr;
		Tree* rawPtr = tree.get();
		trees[rawPtr] = std::move(tree);
		return rawPtr;
	}

	// 移除指定 Tree
	bool removeTree(Tree* tree) {
		return trees.erase(tree) > 0;
	}


	// 获取指定 Tree
	Tree* getTree(Tree* key) const {
		auto it = trees.find(key);
		return it != trees.end() ? it->second.get() : nullptr;
	}

	// 提取Tree（转移所有权）
	std::unique_ptr<Tree> extractTree(Tree* tree) {
		auto it = trees.find(tree);
		if (it != trees.end()) {
			auto extracted = std::move(it->second);
			trees.erase(it);
			return extracted;
		}
		return nullptr;
	}

	// 检查是否包含某个Tree
	bool contains(Tree* tree) const {
		return trees.find(tree) != trees.end();
	}


	// 遍历所有 Tree
	void traverseAllTrees(std::function<void(Tree*)> func) const {
		for (const auto& [key, treePtr] : trees) {
			func(treePtr.get());
		}
	}

	// 遍历所有 Tree
	template<typename Func>
	void forEachTree(Func func) const {
		for (const auto& [key, treePtr] : trees) {
			func(treePtr.get());
		}
	}

	size_t size() const { return trees.size(); }
	bool empty() const { return trees.empty(); }

	// 根据一个 Node 找到它属于哪棵 Tree
	std::pair<Tree*, Node*> findNodeInForest(Node* target) const {
		for (const auto& [key, treePtr] : trees) {
			if (treePtr->contains(target)) {
				return { treePtr.get(), target };
			}
		}
		return { nullptr, nullptr };
	}

	// 根据Node查找所属Tree
	Tree* findTreeByNode(Node* target) const {
		if (!target) return nullptr;

		for (const auto& [key, treePtr] : trees) {
			if (treePtr->contains(target)) {
				return treePtr.get();
			}
		}
		return nullptr;
	}


	// 获取所有Tree的统计信息
	struct ForestStats {
		size_t totalTrees;
		size_t totalNodes;
		size_t totalLeafNodes;
		int maxTreeDepth;
		double averageTreeSize;
	};

	ForestStats getStats() const {
		ForestStats stats{};
		stats.totalTrees = trees.size();

		if (empty()) return stats;

		int maxDepth = -1;
		size_t totalTreeSize = 0;

		for (const auto& [key, treePtr] : trees) {
			auto treeStats = treePtr->getStats();
			stats.totalNodes += treeStats.totalNodes;
			stats.totalLeafNodes += treeStats.leafNodes;
			maxDepth = std::max(maxDepth, treeStats.maxDepth);
			totalTreeSize += treeStats.totalNodes;
		}

		stats.maxTreeDepth = maxDepth;
		stats.averageTreeSize = static_cast<double>(totalTreeSize) / stats.totalTrees;
		return stats;
	}

	// 验证Forest的完整性
	bool validateForest() const {
		for (const auto& [key, treePtr] : trees) {
			if (!treePtr->validateTree()) {
				return false;
			}
		}
		return true;
	}
};



namespace meta {
	// ------------------- 检测容器类型 trait -------------------

	// 通用基类
	template <template<typename...> class T>
	struct ContainerCategoryBase {
		static constexpr const char* name = "unknown";
		static constexpr bool is_map = false;
		static constexpr bool is_multimap = false;
		static constexpr bool is_singlemap = false;
		static constexpr bool is_set = false;
		static constexpr bool is_unordered = false;
	};

	// 主模板继承
	template <template<typename...> class T>
	struct ContainerCategory : ContainerCategoryBase<T> {};



	// ------------------- 各特化 -------------------

	// map
	template <> struct ContainerCategory<std::map> : ContainerCategoryBase<std::map> {
		static constexpr const char* name = "map";
		static constexpr bool is_map = true;
		static constexpr bool is_singlemap = true;
	};

	// multimap
	template <> struct ContainerCategory<std::multimap> : ContainerCategoryBase<std::map> {
		static constexpr const char* name = "map";
		static constexpr bool is_map = true;
		static constexpr bool is_multimap = true;
	};

	// unordered_map
	template <> struct ContainerCategory<std::unordered_map> : ContainerCategoryBase<std::unordered_map> {
		static constexpr const char* name = "unordered_map";
		static constexpr bool is_map = true;
		static constexpr bool is_singlemap = true;
		static constexpr bool is_unordered = true;
	};

	// unordered_multimap
	template <> struct ContainerCategory<std::unordered_multimap> : ContainerCategoryBase<std::unordered_multimap> {
		static constexpr const char* name = "unordered_multimap";
		static constexpr bool is_map = true;
		static constexpr bool is_multimap = true;
		static constexpr bool is_unordered = true;
	};

	// set
	template <> struct ContainerCategory<std::set> : ContainerCategoryBase<std::set> {
		static constexpr const char* name = "set";
		static constexpr bool is_set = true;
	};

	// unordered_set
	template <> struct ContainerCategory<std::unordered_set> : ContainerCategoryBase<std::unordered_set> {
		static constexpr const char* name = "unordered_set";
		static constexpr bool is_set = true;
		static constexpr bool is_unordered = true;
	};



}



// BiMap 使用的映射容器仅为 map 系列
template <
	typename KeyType,
	typename ValueType,
	template <typename...> class ForwardContainer = std::map,
	template <typename...> class ReverseContainer = std::multimap
>
class BiMap {
public:
	using ForwardMap = ForwardContainer<KeyType, ValueType>;	// 定义 类型的容器 的构造
	using ReverseMap = ReverseContainer<ValueType, KeyType>;
	ForwardContainer<KeyType, ValueType> keyToValue;
	ReverseContainer<ValueType, KeyType> valueToKey;

	// ================ constructor ================
	BiMap() = default;

	// ================ operator ================

	BiMap(const BiMap&) = delete;            // 禁止拷贝
	BiMap& operator=(const BiMap&) = delete; // 禁止赋值

	BiMap(BiMap&&) noexcept = default;       // 启用移动构造
	BiMap& operator=(BiMap&&) noexcept = default;


	void insert(const KeyType& k, const ValueType& v) {
		if constexpr (meta::ContainerCategory<ForwardContainer>::is_multimap) {
			keyToValue.insert({ k, v });
		}
		else if constexpr (meta::ContainerCategory<ForwardContainer>::is_map) {
			keyToValue[k] = v;
		}
		else {
			static_assert(
				(meta::ContainerCategory<ForwardContainer>::is_map || meta::ContainerCategory<ForwardContainer>::is_multimap),
				"The input type of ForwardContainer not in BiMap accepted category");

		}

		// reverse 同理
		if constexpr (meta::ContainerCategory<ReverseContainer>::is_multimap) {
			valueToKey.insert({ v, k });
		}
		else if constexpr (meta::ContainerCategory<ReverseContainer>::is_map) {
			valueToKey[v] = k;
		}
		else {
			static_assert(
				(meta::ContainerCategory<ReverseContainer>::is_map || meta::ContainerCategory<ReverseContainer>::is_multimap),
				"The input type of ReverseContainer not in BiMap accepted category");

		}
	}

	bool removeKey(const KeyType& key) {
		return removeGeneric<KeyType, ForwardContainer, ValueType, ReverseContainer>(
			key, keyToValue, valueToKey
		);
	}

	bool removeValue(const ValueType& value) {
		return removeGeneric<ValueType, ReverseContainer, KeyType, ForwardContainer>(
			value, valueToKey, keyToValue
		);
	}

private:

	template<
		typename KeyA, template <typename...> class ContainerA,
		typename ValueB, template <typename...> class ContainerB
	>
	bool removeGeneric(const KeyA& keyToBeDeleted, ContainerA<KeyA, ValueB>& containerA, ContainerB<ValueB, KeyA>& oppositeContainer) {
		std::vector<ValueB> removedValues;

		// step 1 - 逻辑拆分第一步，把 Key 和其对应的 value 删除，并返回已删除的 value 用于在 value 的容器反向删除对应的 key

		bool found = removeGeneric_step1(containerA, keyToBeDeleted, removedValues);
		if (!found) return false;

		// Step 2: 反向同步删除 value → key
		for (ValueB& value : removedValues) {
			removeGeneric_step2(oppositeContainer, value, keyToBeDeleted);
		}
		return true;
	}

	// step 1 - 逻辑拆分第一步，把 Key 和其对应的 value 删除，并返回已删除的 value 用于在 value 的容器反向删除对应的 key
	template <
		template <typename...> class ContainerA,
		typename KeyA,
		typename ValueB
	>
	bool removeGeneric_step1(ContainerA<KeyA, ValueB>& mapA, const KeyA& key, std::vector<ValueB>& removedValues) {
		if constexpr (meta::ContainerCategory<ContainerA>::is_multimap) {
			auto range = mapA.equal_range(key);
			if (range.first == range.second) return false;
			for (auto it = range.first; it != range.second; ) {
				removedValues.push_back(it->second);
				it = mapA.erase(it);
			}
			return true;
		}
		else if constexpr (meta::ContainerCategory<ContainerA>::is_map) {
			auto it = mapA.find(key);
			if (it == mapA.end()) return false;
			removedValues.push_back(it->second);
			mapA.erase(it);
			return true;
		}
		else {
			static_assert(meta::ContainerCategory<ContainerA>::is_map ||
				meta::ContainerCategory<ContainerA>::is_multimap,
				"removeForwardGeneric: MapTypeA must be map or multimap");
		}
	}

	// step 2 — 通用的 “反向删除” 模板
	template <
		template <typename...> class ContainerB,
		typename KeyB,
		typename ValueA
	>
	void removeGeneric_step2(ContainerB<KeyB, ValueA>& mapB, const KeyB& keyB, const ValueA& valueA) {
		if constexpr (meta::ContainerCategory<ContainerB>::is_multimap) {
			auto range = mapB.equal_range(keyB);
			for (auto it = range.first; it != range.second; ) {
				if (it->second == valueA) {
					it = mapB.erase(it);
					break;
				}
				else {
					//std::cout << " skip " << it->second << "\n";
					++it;
				}
			}
		}
		else if constexpr (meta::ContainerCategory<ContainerB>::is_map) {
			mapB.erase(keyB);
		}
		else {
			static_assert(meta::ContainerCategory<ContainerB>::is_map ||
				meta::ContainerCategory<ContainerB>::is_multimap,
				"removeGeneric_step2: ContainerB must be map or multimap");
		}
	}

public:

	void debug_print() const {
		std::cout << "ForwardContainer = " << meta::ContainerCategory<ForwardContainer>::name << "\n";
		std::cout << "ReverseContainer = " << meta::ContainerCategory<ReverseContainer>::name << "\n";

	}

	// -------------------  可选扩展方法 -------------------
	// 为未知类型的容器启用扩展接口
	template <template<typename...> class C = ForwardContainer>
	std::enable_if_t<(!meta::ContainerCategory<C>::is_map)>
		customMethod() const {
		std::cout << "Custom behavior for user-defined container!\n";
	}
};



// ===== Tag 类：实际运行者，管理标签和文件路径 =====
class Tag {
public:

	Forest tagForest;

	NameRegistry TagNameRegistry;	// 给每个标签一个编号，避免大量冗余读写
	NameRegistry filePathRegistry;	// 给每个 path 一个编号

	// 映射表
	std::unordered_map<Node*, long long> tagNodeToTagNameId;	// 将标签树节点转换成 TagName 的唯一标识 (Id) 在 TagNameRegistry 换成原本的名字，o(1) + o(1) + o(1)
	std::unordered_multimap<long long, Node*> tagNameIdToTagNode;

	std::unordered_map<Node*, std::unordered_set<long long>> tagNodeToFilePathIds;	// 将标签树节点转换成数个 FilePathId，代表了该标签有几个 FilePath
	std::unordered_map<long long, std::unordered_set<Node*>> filePathIdToTagNodes;	// 将 FilePathId 转换成数个 标签树节点，代表了该 FilePath 有几个标签




	Tag() = default;

	// ===== 树管理方法 =====
	// 
	// 创建新的标签树
	Tree* createTagTree() {
		return tagForest.addTree();
	}

	std::pair<Tree*, Node*> createTagTreeAndRootNode() {
		Tree* newTree = tagForest.addTree();
		if (!newTree) return { nullptr, nullptr };

		Node* rootNode = newTree->addNode(); // parent = nullptr → root
		if (!rootNode) return { newTree, nullptr };

		return { newTree, rootNode };
	}

	// 获取所有树
	std::vector<Tree*> getAllTrees() const {
		std::vector<Tree*> trees;
		tagForest.forEachTree([&trees](Tree* tree) {
			trees.push_back(tree);
			});
		return trees;
	}

	// 根据节点查找所属树
	Tree* findTreeByNode(Node* node) const {
		return tagForest.findTreeByNode(node);
	}

	// ===== 标签节点管理方法 =====


	// overload 数个版本的 addTag
	// 1. 完整和处理核心 
	// Node* addTag( const std::string& name, Tree* targetTree, Node* tagNode,const std::unordered_set<std::string>& filePaths )
	// 
	// 2. 没有了 Tree 但是有 Node，需要在 forest 里寻找 node 的确切位置进行读写
	// Node* addTag( const std::string& name, , Node* tagNode， const std::unordered_set<std::string>& filePaths )
	// 
	// 3. 没有了 Node 和 Tree，默认开一个新的 Tree 和 Node 进行绑定
	// Node* addTag( const std::string& name， const std::unordered_set<std::string>& filePaths )
	// 


	Node* addTag(
		const std::string& name,
		const std::unordered_set<std::string>& filePaths,
		Node* parentNode,
		Tree* targetTree
	) {
		// 参数验证
		if (name.empty()) { return nullptr; }

		if (!targetTree) { return nullptr; }

		// 验证节点是否在目标树中（如果指定了父节点）
		if (parentNode) {
			Tree* parentTree = findTreeContainingNode(parentNode);
			if (!parentTree || parentTree != targetTree) {
				return nullptr; // 父节点不在目标树中
			}
		}
		return addTagCore(name, filePaths, parentNode, targetTree);
	}

	// 2. 没有了 Tree 但是有 Node
	Node* addTag(
		const std::string& name,
		const std::unordered_set<std::string>& filePaths,
		Node* parentNode
	) {
		// 参数验证
		if (name.empty()) { return nullptr; }

		Tree* targetTree = nullptr;

		if (parentNode) {
			// 在森林中寻找父节点所属的树
			targetTree = findTreeContainingNode(parentNode);
			if (!targetTree) {
				return nullptr; // 找不到父节点所属的树
			}
		}
		else {
			return nullptr;
		}

		return addTagCore(name, filePaths, parentNode, targetTree);
	}

	// 3: 最简版本 - 创建新树和根节点
	Node* addTag(
		const std::string& name,
		const std::unordered_set<std::string>& filePaths
	) {
		// 参数验证
		if (name.empty()) {
			return nullptr;
		}

		// 创建新的树
		Tree* newTree = tagForest.addTree();
		if (!newTree) {
			return nullptr;
		}

		// 在新树中创建根节点
		return addTagCore(name, filePaths, nullptr, newTree);
	}

	// 删除标签节点
	bool removeTagNode(Node* node) {
		if (!node) { return false; }

		// 找到节点所属的树
		Tree* tree = tagForest.findTreeByNode(node);
		if (!tree) {
			return false;
		}

		// 移除映射
		removeNodeMappings(node);

		return tree->removeNode(node);
	}



	// ===== 查询方法 =====

	std::optional<std::string> getTagName(Node* node) const {
		// 优先使用 ID 映射
		auto it = tagNodeToTagNameId.find(node);
		if (it != tagNodeToTagNameId.end()) {
			return TagNameRegistry.getString(it->second);
		}

		return {};
	}

	std::vector<Node*> findTagNodesByName(const std::string& name) const {
		std::vector<Node*> result;

		// 使用 ID 映射查找
		auto tagId = TagNameRegistry.getId(name);	// 使用 Name 获得 NameId
		if (tagId) {	// 表示有这个 NameId
			auto range = tagNameIdToTagNode.equal_range(*tagId);
			for (auto it = range.first; it != range.second; ++it) {
				result.push_back(it->second);
			}
		}

		return result;
	}


	std::vector<Node*> findTagNodesByFilePath(const std::string& filePath) const {
		std::vector<Node*> result;

		// 使用 ID 映射查找
		auto filePathId = filePathRegistry.getId(filePath);
		if (filePathId) {
			auto it = filePathIdToTagNodes.find(*filePathId);
			if (it != filePathIdToTagNodes.end()) {
				result.insert(result.end(), it->second.begin(), it->second.end());
			}
		}

		return result;
	}

	std::unordered_set<std::string> getFilePaths(Node* node) const {
		std::unordered_set<std::string> result;

		// 使用 ID 映射
		auto it = tagNodeToFilePathIds.find(node);
		if (it != tagNodeToFilePathIds.end()) {
			for (auto filePathId : it->second) {
				auto pathOpt = filePathRegistry.getString(filePathId);
				if (pathOpt) {
					result.insert(*pathOpt);
				}
			}
		}


		return result;
	}







	// ===== 修改标签属性 =====
	bool setTagName(Node* node, const std::string& newName) {
		if (!node || newName.empty()) { std::cout << "problem 1 in setTagName \n"; return false; }

		Tree* tree = tagForest.findTreeByNode(node);
		if (!tree) { std::cout << "problem 2 in setTagName \n"; return false; }

		long long newTagId = TagNameRegistry.add(newName);

		auto it = tagNodeToTagNameId.find(node);
		if (it == tagNodeToTagNameId.end()) {
			// 没有绑定过名字 → 新建绑定
			tagNodeToTagNameId[node] = newTagId;
			tagNameIdToTagNode.emplace(newTagId, node);
		}
		else {
			// 已经有旧名字 → 替换绑定
			long long oldTagId = it->second;

			// 从旧 tagId → node 映射里移除
			auto range = tagNameIdToTagNode.equal_range(oldTagId);
			for (auto tagIt = range.first; tagIt != range.second; ) {
				if (tagIt->second == node) {
					tagIt = tagNameIdToTagNode.erase(tagIt);
					break;
				}
				else {
					++tagIt;
				}
			}

			// 更新成新 tagId
			it->second = newTagId;	// tagNodeToTagNameId 的 Node 的映射 Id 改成新的
			tagNameIdToTagNode.emplace(newTagId, node);

			// 清理旧 tagId（如果没人用了）
			if (tagNameIdToTagNode.find(oldTagId) == tagNameIdToTagNode.end()) {
				TagNameRegistry.remove(oldTagId);
			}
		}


		return true;
	}

	bool addFilePath(Node* node, const std::string& filePath) {
		if (!node || filePath.empty()) { return false; }

		Tree* tree = tagForest.findTreeByNode(node);
		if (!tree) { return false; }

		// 更新 ID 映射
		long long filePathId = filePathRegistry.add(filePath);
		bool added = tagNodeToFilePathIds[node].insert(filePathId).second;
		if (added) {
			filePathIdToTagNodes[filePathId].insert(node);
		}

		return added;
	}

	bool removeFilePath(Node* node, const std::string& filePath) {
		if (!node) { return false; }	// 避免 node 是 Null

		Tree* tree = tagForest.findTreeByNode(node);
		if (!tree) { return false; }

		bool removed = false;

		// 更新 ID 映射
		auto filePathId = filePathRegistry.getId(filePath);
		if (filePathId) {
			auto it = tagNodeToFilePathIds.find(node);	// 获得 node 的映射位置
			if (it != tagNodeToFilePathIds.end()) {		// 当 node 存在时才能进行操作
				if (it->second.erase(*filePathId) > 0) {
					removed = true;
					filePathIdToTagNodes[*filePathId].erase(node);	// filePathIdToTagNodes 里删除 Id 对应的 Node
					if (filePathIdToTagNodes[*filePathId].empty()) {// 如果删除后该 Id 指向的 set 为空，表示这个 Id 已经没有存在的必要
						filePathIdToTagNodes.erase(*filePathId);
					}
				}
			}
		}
		return removed;
	}


	// ===== 调试和维护 =====

	void printTagForest() const {
		int treeIndex = 0;
		tagForest.forEachTree([&treeIndex, this](Tree* tree) {
			std::cout << "=== Tree " << treeIndex++ << " ===" << std::endl;
			if (tree->getRoot()) {
				printTagTreeHelper(tree->getRoot(), 0);
			}
			else {
				std::cout << "  (empty)" << std::endl;
			}
			std::cout << std::endl;
			});
	}

	bool empty() const { return tagForest.empty(); }



public:
	// 核心实现方法
	Node* addTagCore(
		const std::string& name,
		const std::unordered_set<std::string>& filePaths,
		Node* parentNode,
		Tree* targetTree
	) {
		// 在目标树中创建节点
		Node* node = targetTree->addNode(parentNode);
		if (!node) { return nullptr; }

		// 使用 NameRegistry 给标签分配 ID
		long long tagId = TagNameRegistry.add(name);

		// 建立标签映射
		tagNodeToTagNameId.emplace(node, tagId);
		tagNameIdToTagNode.emplace(tagId, node);

		// 处理文件路径映射
		for (const auto& filePath : filePaths) {
			long long filePathId = filePathRegistry.add(filePath);	// 为每个 path 注册 Id

			tagNodeToFilePathIds[node].insert(filePathId);	// 将 Id 写入对应 Node
			filePathIdToTagNodes[filePathId].insert(node);	// 将 Node 写入对应 Id
		}


		return node;
	}

	// 辅助方法：在森林中查找包含指定节点的树
	Tree* findTreeContainingNode(Node* node) const {
		if (!node) { return nullptr; }	// 避免传入空 Node

		// 在森林中的其他树中查找
		Tree* foundTree = nullptr;
		tagForest.forEachTree([&foundTree, node](Tree* tree) {
			if (!foundTree && tree->contains(node)) {
				foundTree = tree;
			}
			});

		return foundTree;
	}

	void removeNodeMappings(Node* node) {
		// 移除 tag 映射
		auto itTag = tagNodeToTagNameId.find(node);
		if (itTag != tagNodeToTagNameId.end()) {	// 避免 node 不在映射表（表示找到了）
			long long tagNameId = itTag->second;	// 获得 Id
			auto range = tagNameIdToTagNode.equal_range(tagNameId);
			for (auto it = range.first; it != range.second; ) {	// Id（Name）可能被使用多次
				if (it->second == node) {	// 在里面找自己并删除
					it = tagNameIdToTagNode.erase(it);
				}
				else {
					++it;
				}
			}
			tagNodeToTagNameId.erase(itTag);
		}

		// 移除 file 映射
		auto itFile = tagNodeToFilePathIds.find(node);
		if (itFile != tagNodeToFilePathIds.end()) {	// 避免 node 不在映射表（表示找到了）
			for (auto fileId : itFile->second) {	// 获得 Ids
				filePathIdToTagNodes[fileId].erase(node);
				if (filePathIdToTagNodes[fileId].empty()) {
					filePathIdToTagNodes.erase(fileId);
				}
			}
			tagNodeToFilePathIds.erase(itFile);
		}

	}

	void printTagTreeHelper(Node* node, int indent) const {
		if (!node) return;

		std::string indentStr(indent * 2, ' ');
		auto name = getTagName(node);
		auto filePaths = getFilePaths(node);

		std::cout << indentStr << "- " << (name ? *name : "unnamed")
			<< " (files: " << filePaths.size() << ")" << std::endl;

		for (Node* child : node->children) {
			printTagTreeHelper(child, indent + 1);
		}
	}
};


class FileItem {
public:
	std::string absolutePath;
	unsigned long long bit;	// b，容量 = 2 EB = 2,048 PB = 2,097,152 TB
	bool isDirectory;
};

class Console_1 {
public:
	//static Tag tag;

	Console_1() {

	}

	static void startScreen() {
		using namespace std;
		namespace fs = std::filesystem;

		auto devideRootPath = fs::current_path().root_path();

		Tag tag;

		while (true) {
			cout << "\n\n";
			std::cout << "< startScreen >\n"
				<< "Root : " << devideRootPath
				<< "\n\n___Selection action___  \n"
				<< "1. Your Tag Detail\n";

			string temp;
			cout << "> ";
			getline(cin, temp);

			if (temp == "1") {
				tagScreen(tag);
			}
			else {

				break;
			}
		}


	}

	static void tagScreen(Tag& tag) {
		using namespace std;


		while (true) {
			cout << "\n\n";
			cout << "1. View current Available Tag Tree\n";
			cout << "2. add new tag\n";
			cout << "r. Back\n";

			string temp;
			cout << "> ";
			getline(cin, temp);

			if (temp == "1") {
				Console_1::tagViewScreen(tag);
			}
			else if (temp == "2") {
				Console_1::addNewTag(tag);
			}
			else if (temp == "r") {

				break;
			}
		}
	}


	static void tagViewScreen(Tag& tag) {
		using namespace std;
		while (true) {


			cout << "\n\n";
			cout << "< tagViewScreen >\n";
			// 
			// 有 forest
			// 在 forest 获得 trees，使用每个 tree 的 rootNode
			// 在 tagNodeToTagNameId 使用 rootNode 获得 nameId
			// 在 tagNameRegistry 使用 nameId 获得 name（string）
			// 将 name 一个个放入 set 去重
			// 对 set 里的 name（string）进行排序
			// 
			// os. 现在有了以 name 排序的 set 了，接下来
			// 
			// 在 tagNameRegistry 使用 name 获得 nameId
			// 在 tagNameIdToTagNode 使用 nameId 获得（0 - n）个 node（nodes）
			// 再把获得的 nodes 放入 rootNodes
			// 
			// os. 现在 rootNodes 便有了顺序
			// 
			// 按 rootNodes 的顺序展开所有的 node 
			// 
			// 
			// 使用 printTagTreeHelper(Node* node, int indent) 打印逐个
			// 


			std::set<std::string> rootNames;

			for (auto& [treePtr, treeUPtr] : tag.tagForest.trees) {
				Tree* tree = treePtr;
				if (tree && tree->root) {
					Node* root = tree->root;

					// 映射 root node -> name
					auto it = tag.tagNodeToTagNameId.find(root);
					if (it != tag.tagNodeToTagNameId.end()) {
						long long nameId = it->second;
						rootNames.emplace(*tag.TagNameRegistry.getString(nameId));
					}
				}
			}


			std::vector<Node*> rootNodes;

			for (auto& name : rootNames) {
				// 先去 tagNameRegistry 使用 Name 获得 nameId
				auto idOpt = tag.TagNameRegistry.getId(name);
				if (!idOpt) continue;
				long long nameId = *idOpt;

				// 再去 tagNameIdToTagNode 使用 nameId 获得 (0-n) 个 node
				auto range = tag.tagNameIdToTagNode.equal_range(nameId);
				for (auto it = range.first; it != range.second; ++it) {
					Node* node = it->second;
					if (node && node->isRoot()) {
						rootNodes.push_back(node);
					}
				}
			}

			std::vector < std::pair<Node*, int>> allNodes = getAllNodes(rootNodes);

			int treeCount = 0, nodeCount = 0;
			std::string indentStr;

			// 逐个打印 root tree
			for (auto& [node, deep] : allNodes) {
				++nodeCount;
				if (deep == 0) cout << "\n==== Tree " << ++treeCount << " ====\n";
				indentStr = "";
				for (int i = 0; i <= deep; ++i) { indentStr += "- "; }
				cout << nodeCount << ". " << indentStr << *tag.getTagName(node) << "\n";
			}

			cout << tag.tagForest.size() << "\t" << rootNames.size() << "\t" << rootNodes.size() << "\t" << allNodes.size() << "\n";
			cout << "=== option ===\n";
			cout << "\n1. add sub tag to the tag\n";
			cout << "2. add new tree\n";

			string temp = "";
			cout << "> ";
			getline(cin, temp);


			if (temp == "1") {
				addSubTag(tag, allNodes);
				cout << "chulai\n";
			}
			else if (temp == "2") {
				addNewTag(tag);
			}
			else {
				break;
			}
		}

	}


	// 展开获得每个 root 的所有 node
	static std::vector<std::pair<Node*, int>> getAllNodes(std::vector<Node*>& rootNodes) {
		using namespace std;

		if (rootNodes.size() == 0) return {};

		// 储存每个 node 和他的深度
		vector<pair<Node*, int>> result;

		for (const auto& root : rootNodes) {
			dfs(root, 0, result);
		}

		return result;


	}

	static void dfs(Node* node, int depth, std::vector<std::pair<Node*, int>>& result) {
		if (!node) return;
		result.emplace_back(node, depth); // 存 Node* 与 depth


		for (auto& child : node->children) {
			dfs(child, depth + 1, result);
		}
	}

	static void addSubTag(Tag& tag, std::vector < std::pair<Node*, int>>& allNodes) {

		using namespace std;

		string tagName = "", onWhichNode = "";

		cout << "on  which node \t: ";
		getline(cin, onWhichNode); if (stoi(onWhichNode) - 1 > allNodes.size() || allNodes.size() == 0) return;

		cout << "tag name  \t: ";
		getline(cin, tagName);


		tag.addTag(tagName, {}, allNodes[stoi(onWhichNode) - 1].first);

	}

	static void addNewTag(Tag& tag) {
		using namespace std;

		string tagName = "";

		cout << "tag name : ";

		getline(cin, tagName);

		tag.addTag(tagName, {});

	}
};


class PlayingItem {
public:
	SDL_AudioDeviceID device;

	const std::vector<uint8_t>& pcmData;
	float volume = 1.0f;
	size_t playPos = 0;
	bool isPlaying = true;
	bool loop = false;

	// PlayingItem	-> (1)	PCMData
	// PCMData		-> (N)	PlayingItem

	PlayingItem(const PlayingItem&) = delete;
	PlayingItem& operator=(const PlayingItem&) = delete;

	PlayingItem(PlayingItem&&) noexcept = default;
	PlayingItem& operator=(PlayingItem&&) noexcept = default;

	PlayingItem(const std::vector<uint8_t>& data)
		: pcmData(data) {}


	bool isFinished() { return (playPos >= pcmData.size()); }
	bool isNotFinished() { return (playPos < pcmData.size()); }
	void switchMode() { isPlaying = !isPlaying; }
	void play() { isPlaying = true; }
	void pause() { isPlaying = false; }
	void stop() { isPlaying = false; playPos = 0; }
};
// PlayingItem (n) <-> (n) Device
// PlayingItem	-> (n)	Device
// Device		-> (n)	PlayingItem
//

class OutputDevice {
public:
	SDL_AudioDeviceID outputDeviceId;
	SDL_AudioSpec spec;


	void setOutputDevice(
		const char* device = nullptr
	) {
		outputDeviceId = SDL_OpenAudioDevice(device, 0, &spec, nullptr, 0);
		return;
	}
};

class Device {
public:
	std::vector<OutputDevice> outputDevices;
	// Device	(1) <-> (n)	OutputDevice
	// Device		-> (n)	OutputDevice
	// OutputDevice	-> (1)	Device
};

namespace Token {



	// 用于对状态进行标注
	// 可用于多线程场景
	template<typename Derived>
	class TokenAvailableImpl {
	public:
		std::atomic<bool> isAvailable{ true };
		// ================= constructor =================

		TokenAvailableImpl() = default;

		TokenAvailableImpl(bool isAvailable) noexcept : isAvailable(isAvailable) {}

		// ================= operator =================

		// 复制
		TokenAvailableImpl(const TokenAvailableImpl& other) noexcept { copy(other); }
		TokenAvailableImpl& operator=(const TokenAvailableImpl& other) noexcept {
			if (this != &other) { copy(other); }
			return *this;
		}

		// 移动构造
		TokenAvailableImpl(TokenAvailableImpl&& other) noexcept { copy(other); }

		// 移动赋值
		TokenAvailableImpl& operator=(TokenAvailableImpl&& other) noexcept {
			if (this != &other) { copy(other); }
			return *this;
		}

		// ================= method =================

		// 尝试获取资源，如果成功返回 true
		// (仅当为 true 时返回 true 并转换为 false)
		bool tryAcquire() noexcept {
			bool expected = true;
			return isAvailable.compare_exchange_strong(expected, false, std::memory_order_acquire, std::memory_order_relaxed);
		}

		// 转换为 true
		void releaseAcquired() noexcept {
			isAvailable.store(true, std::memory_order_release);

			// 需要检测释放多余调用可以解注释以下
			//if(!success){ std::cerr << "Repeated calls TokenAvailableImpl::releaseAcquired !!!\n" }
		}
		// 查询状态
		bool getIsAvailable() const noexcept { return isAvailable.load(std::memory_order_relaxed); }

		// 写入状态
		void setIsAvailable(bool exptected) {
			isAvailable.store(exptected, std::memory_order_release);
		}
	private:
		void copy(TokenAvailableImpl& destination, TokenAvailableImpl& source) noexcept {
			bool val = source.isAvailable.load(std::memory_order_relaxed);
			destination.isAvailable.store(val, std::memory_order_relaxed);
		}

		void copy(TokenAvailableImpl& source) {
			isAvailable.store(source.isAvailable.load(std::memory_order_relaxed), std::memory_order_relaxed);
		}

		void copy(const TokenAvailableImpl& source) {
			isAvailable.store(source.isAvailable.load(std::memory_order_relaxed), std::memory_order_relaxed);
		}
	};
	class TokenAvailable : public TokenAvailableImpl<TokenAvailable> {
		using Base = TokenAvailableImpl<TokenAvailable>;
	public:
		TokenAvailable() = default;
		TokenAvailable(bool isAvailable) : Base(isAvailable) {}
	};

	template<typename Derived>
	class TokenAvailableVectorImpl {
	public:
		std::vector<TokenAvailable> tokens;
		std::shared_mutex& sharedMtxFromOwner;

		// ================= constructor =================

		TokenAvailableVectorImpl() = default;

		TokenAvailableVectorImpl(std::shared_mutex& sharedMtxFromOwner) :sharedMtxFromOwner(sharedMtxFromOwner) {}

		// ================= operator =================

		// 禁止复制构造
		TokenAvailableVectorImpl(const TokenAvailableVectorImpl&) = delete;

		// 复制赋值
		TokenAvailableVectorImpl& operator=(TokenAvailableVectorImpl& other) {
			copy(other);
			return *this;
		}

		// 允许移动
		TokenAvailableVectorImpl(TokenAvailableVectorImpl&&) noexcept = default;
		TokenAvailableVectorImpl& operator=(TokenAvailableVectorImpl&&) noexcept = default;


		// ================= method =================

		// 尝试获取资源，
		// 成功返回 { true  , index }
		// 失败返回 { false , -1 }
		std::pair<bool, int> tryAcquire() {
			{
				std::shared_lock readLock(sharedMtxFromOwner);
				int i = 0;
				for (i = 0; i < tokens.size(); i++) {
					if (tokens[i].tryAcquire()) {
						return { true, i };
					}
				}
				return { false, -1 };
			}
		}

		// 尝试获取资源，
		// 成功返回 { true  }
		// 失败返回 { false }
		bool tryAcquire(int& resultIndex) {
			{
				std::shared_lock readLock(sharedMtxFromOwner);
				int i = 0;
				for (i = 0; i < tokens.size(); i++) {
					if (tokens[i].tryAcquire()) {
						resultIndex = i;
						return { true };
					}
				}
				return { false };
			}
		}

		// 释放资源
		void releaseAcquired(int index) {
			std::shared_lock readLock(sharedMtxFromOwner);
			// 如果需要检测可以解注释以下（一般正确使用不会出错）
			// if (index >= tokens.size()) { std::cout << "Error !!!"; return ; }
			tokens[index].releaseAcquired();
		}
	private:
		void copy(TokenAvailableVectorImpl& destination, TokenAvailableVectorImpl& source) {
			std::vector<bool> temp;
			{
				std::shared_lock<std::shared_mutex> readLock(source.sharedMtxFromOwner);
				temp.reserve(source.tokens.size());
				for (TokenAvailable tk : source.tokens) {
					temp.emplace_back(tk.getIsAvailable());
				}
			}
			{
				std::shared_lock<std::shared_mutex> writeLock(destination.sharedMtxFromOwner);

				destination.tokens.clear();
				destination.tokens.reserve(temp.size());
				for (bool b : temp) {
					destination.tokens.emplace_back(b);
				}
			}
		}

		void copy(TokenAvailableVectorImpl& source) {
			copy(*this, source);
		}
	};
	class TokenAvailableVector : public TokenAvailableVectorImpl<TokenAvailableVector> {};

	template<
		typename Derived,
		template<typename...> class MapType = std::map,
		typename Key = std::string
	>
	class TokenAvailableMapImpl {
	public:
		MapType<Key, TokenAvailable> tokens;
		std::shared_mutex& sharedMtxFromOwner;

		// ================= constructor =================

		TokenAvailableMapImpl() = delete;

		TokenAvailableMapImpl(std::shared_mutex& sharedMtxFromOwner) :sharedMtxFromOwner(sharedMtxFromOwner) {}

		// ================= operator =================

		// 禁止复制构造
		TokenAvailableMapImpl(const TokenAvailableMapImpl&) = delete;

		// 复制赋值
		TokenAvailableMapImpl& operator=(TokenAvailableMapImpl& other) {
			// 把另一个的数据赋值过来，不影响源数据
			copy(other);
			return *this;
		}

		// 允许移动
		TokenAvailableMapImpl(TokenAvailableMapImpl&&) noexcept = default;
		TokenAvailableMapImpl& operator=(TokenAvailableMapImpl&&) noexcept = default;


		// ================= method =================

		// 尝试获取资源，
		// 成功返回 { true  , key }
		// 失败返回 { false , null key }
		std::pair<bool, Key> tryAcquire() {
			std::shared_lock readLock(sharedMtxFromOwner);
			for (auto& [key, tk] : tokens) {
				if (tk.tryAcquire()) {
					return { true, key };
				}
			}
			return { false, Key{} };

		}

		// 尝试获取指定资源，
		// 成功返回 { true  }
		// 失败返回 { false }
		bool tryAcquire(Key& key) {

			std::shared_lock readLock(sharedMtxFromOwner);
			auto it = tokens.find(key);
			if (it == tokens.end()) return false;
			return it->second.tryAcquire();
		}

		// 尝试获取指定资源，
		// 成功返回 { true  }
		// 失败返回 { false }
		bool tryAcquire(const Key& key) {
			Key temp = key;
			return tryAcquire(temp);
		}


		// 释放资源
		void releaseAcquired(Key& key) {
			std::shared_lock readLock(sharedMtxFromOwner);
			// 如果需要检测可以解注释以下（一般正确使用不会出错）
			auto it = tokens.find(key);
			if (it == tokens.end()) { std::cout << "Error !!! [releaseAcquired]"; return; }

			tokens[key].releaseAcquired();
		}
	private:
		void copy(TokenAvailableMapImpl& destination, TokenAvailableMapImpl& source) {
			if (&destination == &source) return;


			MapType<Key, bool> temp;
			{
				std::shared_lock<std::shared_mutex> readLock(source.sharedMtxFromOwner);
				for (auto& [key, tk] : source.tokens) {
					temp[key] = tk.getIsAvailable();
				}
			}
			{
				std::unique_lock writeLock(destination.sharedMtxFromOwner);
				destination.tokens.clear();
				if constexpr (requires { destination.tokens.reserve(temp.size()); }) {
					destination.tokens.reserve(temp.size()); // 只有 unordered_map 才能这样
				}
				for (auto& [key, available] : temp) {
					destination.tokens.emplace(key, TokenAvailable(available));
				}
			}
		}

		void copy(TokenAvailableMapImpl& source) {
			copy(*this, source);
		}
	};

	template<
		template<typename...> class MapType = std::map,
		typename Key = std::string
	>
	class TokenAvailableMap :
		public TokenAvailableMapImpl<TokenAvailableMap<MapType, Key>, MapType, Key>
	{
	public:
		using Base = TokenAvailableMapImpl<TokenAvailableMap<MapType, Key>, MapType, Key>;

		// ================= constructor =================

		explicit TokenAvailableMap(std::shared_mutex& mtx) : Base(mtx) {}
	};

	template<typename Derived, typename T>
	class AtomicTokenImpl {
	public:
		std::atomic<T> atomicToken;

		// ================= constructor =================

		AtomicTokenImpl() = default;

		AtomicTokenImpl(T t) : atomicToken(t){}

		// ================= operator =================

	};


	// ================= ReadGuard =================


	template<typename Container>
	class ReadGuard {
		using shared_mutex = std::shared_mutex;

		std::shared_lock<shared_mutex> lock_;
		const Container& container_;

	public:
		ReadGuard(std::shared_ptr<shared_mutex> mtx, const Container& container)
			: lock_(*mtx), container_(container) {}

		ReadGuard(shared_mutex& mtx, const Container& container)
			: lock_(mtx), container_(container) {}

		const Container& get() const { return container_; }
		const Container* operator->() const { return &container_; }
		const Container& operator*() const { return container_; }
	};

	template<typename Container>
	class WriteGuard {
		using shared_mutex = std::shared_mutex;

		std::unique_lock<std::shared_mutex> lock_;
		Container& container_;

	public:
		WriteGuard(shared_mutex& mtx, Container& container)
			: lock_(mtx), container_(container) {}

		WriteGuard(std::shared_ptr<shared_mutex> mtx, Container& container) : lock_(*mtx), container_(container) {}

		Container& get() { return container_; }
		Container* operator->() { return &container_; }
		Container& operator*() { return container_; }
	};


	template<
		typename Derived,
		typename Value
	>
	class ThreadVectorImpl {
		using shared_mutex = std::shared_mutex;
		using shared_lock = std::shared_lock<shared_mutex>;
		using unique_lock = std::unique_lock<shared_mutex>;
		using size_type = typename std::vector<Value>::size_type;
	public:
		std::vector<Value> tVector;
		std::shared_ptr<shared_mutex> tMtx;

		// ================= constructor =================

		ThreadVectorImpl() : tVector(), tMtx(std::make_shared<shared_mutex>()) {}

		explicit ThreadVectorImpl(size_type count)
			: tVector(count), tMtx(std::make_shared<shared_mutex>()) {}

		ThreadVectorImpl(size_type count, const Value& value)
			: tVector(count, value), tMtx(std::make_shared<shared_mutex>()) {}


		// 复制并使用 other 的同一把锁
		ThreadVectorImpl(std::shared_ptr<shared_mutex> other_tMtx) : tVector(), tMtx(other_tMtx) {}


		// 复制构造
		ThreadVectorImpl(const ThreadVectorImpl& other) {
			// 完全复制 other 的锁与数据
			if (other.tMtx) {
				std::shared_lock readLock(*other.tMtx);
				tVector = other.tVector;
				tMtx = other.tMtx; // 拷贝共享锁（shared_ptr），保证引用计数正确
			}
			else {
				// 允许 other 是空锁状态（默认构造）
				tVector = other.tVector;
				tMtx = std::make_shared<shared_mutex>();
			}
		}
		ThreadVectorImpl& operator=(const ThreadVectorImpl& other) {
			// 数据复制：不复制 mutex
			if (this == &other) return *this;


			if (other.tMtx == tMtx) {	// 如果共享同一把锁
				unique_lock writeLock(*tMtx);
				tVector = other.tVector;
			}
			else {						// 如果锁不同
				// 锁不同：同时锁住两个对象
				if (this < &other) {
					std::scoped_lock lock(*tMtx, *other.tMtx);
					tVector = other.tVector;
					// 不改变 tMtx！
				}
				else {
					std::scoped_lock lock(*other.tMtx, *tMtx);
					tVector = other.tVector;
					// 不改变 tMtx！
				}
			}
			return *this;
		}


		// 移动构造：转移数据和锁
		ThreadVectorImpl(ThreadVectorImpl&& other) {
			if (other.tMtx) {
				unique_lock writeLock(*other.tMtx);
				tVector = std::move(other.tVector);
				tMtx = std::move(other.tMtx);
			}
			else {
				tVector = std::move(other.tVector);
				tMtx = std::make_shared<shared_mutex>();
			}
		}

		// 移动赋值：只转移数据，不改变锁
		ThreadVectorImpl& operator=(ThreadVectorImpl&& other) {
			if (this == &other) return *this;

			if (other.tMtx == tMtx) {
				// 共享同一把锁
				unique_lock writeLock(*tMtx);
				tVector = std::move(other.tVector);
			}
			else {
				// 锁不同：同时锁住两个对象
				if (this < &other) {
					std::scoped_lock lock(*tMtx, *other.tMtx);
					tVector = std::move(other.tVector);
					// 不改变 tMtx！
				}
				else {
					std::scoped_lock lock(*other.tMtx, *tMtx);
					tVector = std::move(other.tVector);
					// 不改变 tMtx！
				}
			}
			return *this;
		}
		// ================= operator =================

		Value& operator[](size_t idx) {
			shared_lock readLock(*tMtx);
			if (idx >= tVector.size())
				throw std::out_of_range("ThreadVectorImpl: index out of range");
			return tVector[idx];
		}

		const Value& operator[](size_t idx) const {
			shared_lock readLock(*tMtx);
			if (idx >= tVector.size())
				throw std::out_of_range("ThreadVectorImpl: index out of range");
			return tVector[idx];
		}

		// ================= method =================

		// push_back
		void push_back(const Value& val) {
			unique_lock writeLock(*tMtx);
			tVector.push_back(val);
		}

		void push_back(Value&& val) {
			unique_lock writeLock(*tMtx);
			tVector.push_back(std::move(val));
		}

		// emplace_back
		template<typename... Args>
		Value& emplace_back(Args&&... args) {
			unique_lock writeLock(*tMtx);
			return tVector.emplace_back(std::forward<Args>(args)...);
		}

		// emplace
		template<typename... Args>
		auto emplace(typename std::vector<Value>::iterator pos, Args&&... args) {
			unique_lock writeLock(*tMtx);
			return tVector.emplace(pos, std::forward<Args>(args)...);
		}

		// at
		Value& at(size_t idx) {
			shared_lock readLock(*tMtx);
			return tVector.at(idx);
		}

		const Value& at(size_t idx) const {
			shared_lock readLock(*tMtx);
			return tVector.at(idx);
		}

		// resize
		void resize(size_t newSize) {
			unique_lock writeLock(*tMtx);
			tVector.resize(newSize);
		}

		void resize(size_t newSize, const Value& val) {
			unique_lock writeLock(*tMtx);
			tVector.resize(newSize, val);
		}

		// reserve
		void reserve(size_t newCap) {
			unique_lock writeLock(*tMtx);
			tVector.reserve(newCap);
		}


		// 尝试收缩容量以匹配大小
		void shrink_to_fit() {
			unique_lock writeLock(*tMtx);
			tVector.shrink_to_fit();
		}

		// capacity
		size_t capacity() const {
			shared_lock readLock(*tMtx);
			return tVector.capacity();
		}

		// erase
		auto erase(typename std::vector<Value>::iterator pos) {
			unique_lock writeLock(*tMtx);
			return tVector.erase(pos);
		}

		auto erase(typename std::vector<Value>::iterator first, typename std::vector<Value>::iterator last) {
			unique_lock writeLock(*tMtx);
			return tVector.erase(first, last);
		}

		// insert
		auto insert(typename std::vector<Value>::iterator pos, const Value& val) {
			unique_lock writeLock(*tMtx);
			return tVector.insert(pos, val);
		}

		auto insert(typename std::vector<Value>::iterator pos, Value&& val) {
			unique_lock writeLock(*tMtx);
			return tVector.insert(pos, std::move(val));
		}

		// pop_back
		void pop_back() {
			unique_lock writeLock(*tMtx);
			if (tVector.empty())
				throw std::out_of_range("ThreadVectorImpl: pop_back() on empty vector");
			tVector.pop_back();
		}

		// clear
		void clear() {
			unique_lock writeLock(*tMtx);
			tVector.clear();
		}

		// ================= size / empty =================
		size_t size() const noexcept {
			shared_lock readLock(*tMtx);
			return tVector.size();
		}

		bool empty() const noexcept {
			shared_lock readLock(*tMtx);
			return tVector.empty();
		}

		// ================= swap =================
		void swap(ThreadVectorImpl& other) {
			if (this == &other) return;

			if (tMtx == other.tMtx) {
				// 共享同一把锁：只需要一个锁
				unique_lock lock(*tMtx);
				tVector.swap(other.tVector);
			}
			else {
				// 不同的锁：按地址排序避免死锁
				if (this < &other) {
					std::scoped_lock lock(*tMtx, *other.tMtx);
					tVector.swap(other.tVector);
				}
				else {
					std::scoped_lock lock(*other.tMtx, *tMtx);
					tVector.swap(other.tVector);
				}
			}
		}

		// front/back
		Value& front() {
			shared_lock lock(*tMtx);
			if (tVector.empty()) throw std::out_of_range("ThreadVectorImpl: front() called on empty vector");
			return tVector.front();
		}

		const Value& front() const {
			shared_lock lock(*tMtx);
			if (tVector.empty()) throw std::out_of_range("ThreadVectorImpl: front() called on empty vector");
			return tVector.front();
		}

		Value& back() {
			shared_lock lock(*tMtx);
			if (tVector.empty()) throw std::out_of_range("ThreadVectorImpl: back() called on empty vector");
			return tVector.back();
		}

		const Value& back() const {
			shared_lock lock(*tMtx);
			if (tVector.empty()) throw std::out_of_range("ThreadVectorImpl: back() called on empty vector");
			return tVector.back();
		}

		// ================= RAII 访问器（用于需要多次操作的场景）=================

		ReadGuard<std::vector<Value>> read_lock() const {
			return ReadGuard<std::vector<Value>>(tMtx, tVector);
		}

		WriteGuard<std::vector<Value>> write_lock() {
			return WriteGuard<std::vector<Value>>(tMtx, tVector);
		}

		// 获取原始 vector（慎用）
		std::vector<Value>& unsafe_ref() { return tVector; }
		const std::vector<Value>& unsafe_ref() const { return tVector; }

		// 手动上锁访问器
		shared_mutex& mutex() { return *tMtx; }
		const shared_mutex& mutex() const { return *tMtx; }

	private:
	};

	template<typename Value>
	class ThreadVector : public ThreadVectorImpl<ThreadVector<Value>, Value> {
		using Base = ThreadVectorImpl<ThreadVector<Value>, Value>;
		using size_type = std::vector<Value>::size_type;

	public:
		ThreadVector() = default;

		ThreadVector(size_type size) : Base(size) {}
	};

	template<
		typename Derived,
		template<typename...> class MapType = std::map,
		typename Key = std::string,
		typename Value = std::string
	>
	class MultiThreadMapImpl {
		using shared_mutex = std::shared_mutex;
		using shared_lock = std::shared_lock<shared_mutex>;
		using unique_lock = std::unique_lock<shared_mutex>;
	public:
		MapType<Key, Value> tMap;
		std::shared_ptr<shared_mutex> tMtx;

		// ================= constructor =================

		// default 独立全新构造数据
		MultiThreadMapImpl() : tMap(), tMtx(std::make_shared<shared_mutex>()) {}

		// 复制
		explicit MultiThreadMapImpl(std::shared_ptr<shared_mutex> sharedLock) :
			tMap(),
			tMtx(sharedLock)	// 共同使用同一个锁
		{}

		// copy 构造: 完全复制 
		MultiThreadMapImpl(const MultiThreadMapImpl& other) {
			if (other.tMtx) {
				shared_lock readLock(*other.tMtx);
				tMap = other.tMap;
				tMtx = other.tMtx;
			}
			else {
				tMap = other.tMap;
				tMtx = std::make_shared<shared_mutex>();
			}
		}

		// 拷贝赋值：只复制数据，不改变锁
		MultiThreadMapImpl& operator=(const MultiThreadMapImpl& other) {
			if (this == &other) return *this;

			if (other.tMtx == tMtx) {
				// 共享同一把锁
				unique_lock writeLock(*tMtx);
				tMap = other.tMap;
			}
			else {
				// 锁不同：同时锁住两个对象
				if (this < &other) {
					std::scoped_lock lock(*tMtx, *other.tMtx);
					tMap = other.tMap;
					// 不改变 tMtx！
				}
				else {
					std::scoped_lock lock(*other.tMtx, *tMtx);
					tMap = other.tMap;
					// 不改变 tMtx！
				}
			}
			return *this;
		}

		// 移动构造：转移数据和锁
		MultiThreadMapImpl(MultiThreadMapImpl&& other) noexcept {
			if (other.tMtx) {
				unique_lock lock(*other.tMtx);
				tMap = std::move(other.tMap);
				tMtx = std::move(other.tMtx);
			}
			else {
				tMap = std::move(other.tMap);
				tMtx = std::make_shared<shared_mutex>();
			}
		}

		// 移动赋值：只转移数据，不改变锁
		MultiThreadMapImpl& operator=(MultiThreadMapImpl&& other) {
			if (this == &other) return *this;
			if (other.tMtx == tMtx) {
				// 共享同一把锁
				unique_lock writeLock(*tMtx);
				tMap = std::move(other.tMap);
			}
			else {
				// 锁不同：同时锁住两个对象
				if (this < &other) {
					std::scoped_lock lock(*tMtx, *other.tMtx);
					tMap = std::move(other.tMap);
					// 不改变 tMtx！
				}
				else {
					std::scoped_lock lock(*other.tMtx, *tMtx);
					tMap = std::move(other.tMap);
					// 不改变 tMtx！
				}
			}
			return *this;
		}
		// ================= operator =================

		// operator[] 模拟 std::map::operator[] 行为：若不存在则创建并返回引用
		std::enable_if_t<meta::ContainerCategory<MapType>::is_singlemap, Value&>
			operator[](const Key& key) {
			return getValueRef(key);
		}

		std::enable_if_t<meta::ContainerCategory<MapType>::is_singlemap, const Value&>
			operator[](const Key& key) const {
			shared_lock readLock(*tMtx);
			return tMap.at(key);  // 若不存在会抛异常，与 std::map 行为一致
		}

		// ================= method =================

		// convenience: run a callable under shared lock, returns callable's return value
		template<typename Fn>
		auto with_read(Fn&& fn) const -> decltype(fn(tMap)) {
			shared_lock readLock(*tMtx);
			return fn(tMap);
		}

		// convenience: run a callable under unique lock
		template<typename Fn>
		auto with_write(Fn&& fn) -> decltype(fn(tMap)) {
			unique_lock writeLock(*tMtx);
			return fn(tMap);
		}

		// ================= Insert / Update =================

		// 在 map 里插入或覆盖已有值
		// 在 multimap, 如果该 key 和 value 没有同时出现映射过才插入
		Value& insert_or_assign(const Key& key, const Value& value) {
			if constexpr (meta::ContainerCategory<MapType>::is_multimap) {
				{
					// multimap：仅当 (key, value) 组合不存在时才插入
					shared_lock readLock(*tMtx);	// 使用读寻找，找不到时才使用写
					for (auto range = tMap.equal_range(key);
						range.first != range.second; ++range.first) {
						if (range.first->second == value) {
							// 已存在相同 (key,value)，直接返回
							return range.first->second;
						}
					}
				}
				{
					// multimap：仅当 (key, value) 组合不存在时才插入
					unique_lock writeLock(*tMtx);	// 重新寻找一遍，避免在换锁的中途其他线程已经插入
					for (auto range = tMap.equal_range(key);
						range.first != range.second; ++range.first) {
						if (range.first->second == value) {
							// 已存在相同 (key,value)，直接返回
							return range.first->second;
						}
					}
					// 没有同样的 key-value，插入新元素
					auto it = tMap.emplace(key, value);
					return it->second;
				}
			}
			else {
				unique_lock writeLock(*tMtx);
				auto [it, _] = tMap.insert_or_assign(key, value);
				return it->second;

			}
		}

		Value& insert_or_assign(Key&& key, Value&& value) {
			if constexpr (meta::ContainerCategory<MapType>::is_multimap) {
				{
					shared_lock readLock(*tMtx);
					auto [begin, end] = tMap.equal_range(key);
					for (auto it = begin; it != end; ++it) {
						if (it->second == value)
							return it->second;
					}
				}
				{
					unique_lock writeLock(*tMtx);
					auto [begin, end] = tMap.equal_range(key);
					for (auto it = begin; it != end; ++it) {
						if (it->second == value)
							return it->second;
					}
					auto it = tMap.emplace(std::move(key), std::move(value));
					return it->second;
				}
			}
			else {
				unique_lock writeLock(*tMtx);
				auto [it, _] = tMap.insert_or_assign(std::move(key), std::move(value));
				return it->second;
			}
		}

		// 在 map 如果该 key 已存在，则不会插入，也不会修改旧值。
		// 在 multimap 永远插入一个新的元素（即使 key 已存在）
		Value& insert(const Key& key, const Value& value) {
			unique_lock writeLock(*tMtx);
			if constexpr (meta::ContainerCategory<MapType>::is_multimap) {
				auto it = tMap.emplace(key, value); // multimap emplace -> iterator
				return it->second;
			}
			else {
				// use explicit value_type to avoid MSVC initializer-list ambiguity
				auto [it, inserted] = tMap.insert(std::pair<Key, Value>(key, value));
				return it->second;
			}
		}

		// 在 map 如果该 key 已存在，则不会插入，也不会修改旧值。
		// 在 multimap 永远插入一个新的元素（即使 key 已存在）
		Value& insert(Key&& key, Value&& value) {
			unique_lock writeLock(*tMtx);
			if constexpr (meta::ContainerCategory<MapType>::is_multimap) {
				auto it = tMap.emplace(std::move(key), std::move(value));
				return it->second;
			}
			else {
				auto [it, inserted] = tMap.insert(std::pair<Key, Value>(std::move(key), std::move(value)));
				return it->second;
			}
		}

		template<typename Pair>
		Value& insert(Pair&& pair) {
			unique_lock writeLock(*tMtx);

			if constexpr (meta::ContainerCategory<MapType>::is_multimap) {
				auto it = tMap.insert(std::forward<Pair>(pair));
				return it->second;
			}
			else {
				auto [it, inserted] = tMap.insert(std::forward<Pair>(pair));
				return it->second;
			}
		}


		template<typename... Args>
		Value& emplace(Args&&... args) {
			unique_lock writeLock(*tMtx);
			if constexpr (meta::ContainerCategory<MapType>::is_multimap) {
				auto it = tMap.emplace(std::forward<Args>(args)...); // iterator
				return it->second;
			}
			else {
				auto [it, inserted] = tMap.emplace(std::forward<Args>(args)...); // pair
				return it->second;
			}
		}


		// origin removeKey
		bool removeKey(const Key& key) {

			if constexpr (meta::ContainerCategory<MapType>::is_singlemap) {
				unique_lock writeLock(*tMtx);
				auto it = tMap.find(key);
				if (it == tMap.end()) return false;
				tMap.erase(it);
				return true;
			}
			return false;
		}



		// origin getValuePtr
		// 使用 key 获得对应 value 的指针, key 不存在则返回 nullptr
		Value* getValuePtr(const Key& key) {
			if constexpr (meta::ContainerCategory<MapType>::is_singlemap) {
				shared_lock readLock(*tMtx);
				auto it = tMap.find(key);
				if (it == tMap.end()) return nullptr;	// 如果没找到，返回 nullptr
				return &it->second;
			}
			return nullptr;
		}



		// origin getValueRef
		// PS: 获得的引用在外部属于不安全状态, 使用时记得搭配 mutex
		// 使用 key 获得对应 value 的引用, key 不存在则插入该 key 并使用 value 的默认构造
		Value& getValueRef(const Key& key) {
			if constexpr (meta::ContainerCategory<MapType>::is_singlemap) {
				// 先找看这个 key 有没有对应的 value，找到了就返回 value&
				// 没有就上锁并再找一次避免中间的空挡这个 key 被插入，还是没有就插入 value 的默认构造并返回这个 value&
				{
					shared_lock readLock(*tMtx);
					auto it = tMap.find(key);
					if (it != tMap.end()) { return it->second; }
				}
				{
					unique_lock writeLock(*tMtx);
					auto it = tMap.find(key);
					if (it != tMap.end()) { return it->second; }
					else {
						auto [newIt, inserted] = tMap.try_emplace(key);
						return newIt->second;
					}
				}
			}
		}
		// ================= getKeys / getValues =================

		// 获取所有键
		std::vector<Key> keys() const {
			std::shared_lock readLock(*tMtx);
			std::vector<Key> result;
			result.reserve(tMap.size());
			for (const auto& [key, value] : tMap) {
				result.push_back(key);
			}
			return result;
		}

		// 获取所有值
		std::vector<Value> values() const {
			shared_lock readLock(*tMtx);
			std::vector<Value> result;
			result.reserve(tMap.size());
			for (const auto& [key, value] : tMap) {
				result.push_back(value);
			}
			return result;
		}
		// ================= get_or_insert =================

		Value& get_or_insert(const Key& key) {
			if constexpr (meta::ContainerCategory<MapType>::is_multimap) {
				// multimap: 永远插入新的 key
				unique_lock writeLock(*tMtx);
				auto it = tMap.emplace(key, Value{});
				return it->second;
			}
			else {
				// map: 有则返回，无则插入默认值
				{
					shared_lock readLock(*tMtx);
					auto it = tMap.find(key);
					if (it != tMap.end()) {
						return it->second;
					}
				}
				{
					unique_lock writeLock(*tMtx);
					// double-check 避免换锁间被插入
					auto it = tMap.find(key);
					if (it != tMap.end()) {
						return it->second;
					}
					auto [newIt, inserted] = tMap.try_emplace(key);
					return newIt->second;
				}
			}
		}

		template<typename F>
		Value& get_or_insert_with(const Key& key, F&& make_value) {
			if constexpr (meta::ContainerCategory<MapType>::is_multimap) {
				unique_lock writeLock(*tMtx);
				auto it = tMap.emplace(key, make_value());
				return it->second;
			}
			else {
				{
					shared_lock readLock(*tMtx);
					auto it = tMap.find(key);
					if (it != tMap.end())
						return it->second;
				}
				{
					unique_lock writeLock(*tMtx);
					auto it = tMap.find(key);
					if (it != tMap.end())
						return it->second;
					auto [newIt, inserted] = tMap.emplace(key, make_value());
					return newIt->second;
				}
			}
		}


		// ================= RAII 访问器 =================

		ReadGuard<MapType<Key, Value>> read_lock() const {
			return ReadGuard<MapType<Key, Value>>(tMtx, tMap);
		}

		WriteGuard<MapType<Key, Value>> write_lock() {
			return WriteGuard<MapType<Key, Value>>(tMtx, tMap);
		}

		// ================= Contains =================
		bool contains(const Key& key) const {
			shared_lock readLock(*tMtx);
			return tMap.find(key) != tMap.end();
		}

		// ================= Erase =================
		bool erase(const Key& key) {
			unique_lock writeLock(*tMtx);
			return tMap.erase(key) > 0;
		}

		// ================= Clear =================
		void clear() {
			unique_lock writeLock(*tMtx);
			tMap.clear();
		}

		// ================= Size =================
		size_t size() const {
			shared_lock readLock(*tMtx);
			return tMap.size();
		}

		// ================= swap =================

		void swap(MultiThreadMapImpl& other) {
			if (this == &other) return;

			if (tMtx == other.tMtx) {
				// 共享同一把锁：只需要一个锁
				unique_lock lock(*tMtx);
				tMap.swap(other.tMap);
			}
			else {
				// 不同的锁：按地址排序避免死锁
				if (this < &other) {
					std::scoped_lock lock(*tMtx, *other.tMtx);
					tMap.swap(other.tMap);
				}
				else {
					std::scoped_lock lock(*other.tMtx, *tMtx);
					tMap.swap(other.tMap);
				}
			}
		}
	private:
	};

	template<
		template<typename...> class MapType = std::map,
		typename Key = std::string,
		typename Value = std::string
	>
	class ThreadMap :
		public MultiThreadMapImpl<ThreadMap<MapType, Key, Value>, MapType, Key, Value>
	{
		using Base = MultiThreadMapImpl<ThreadMap<MapType, Key, Value>, MapType, Key, Value>;
		using shared_mutex = std::shared_mutex;
		using shared_lock = std::shared_lock<shared_mutex>;
		using unique_lock = std::unique_lock<shared_mutex>;
	public:
		// ================= constructor =================

		ThreadMap() = default;

		ThreadMap(std::shared_ptr<shared_mutex> mtx) : Base(mtx) {}

		ThreadMap(const ThreadMap&) noexcept = default;
		ThreadMap& operator=(const ThreadMap&) noexcept = default;
		ThreadMap(ThreadMap&&) noexcept = default;
		ThreadMap& operator=(ThreadMap&&) noexcept = default;

	};



}


namespace FFmpegTool {
	using string = std::string;

	// 使用 stream 和它的 fmtCtx 检测 
	// 返回 stream 的时长
	double getStreamDurationSec(const AVFormatContext* fmtCtx, const AVStream* stream);

	string getChannelDescription(const AVCodecParameters* codecpar);

	// 赋值 fmtCtx with path
	bool AVFormatContext_open(AVFormatContext*& fmtCtx, const char* path);

	void AVFormatContext_close(AVFormatContext*& fmtCtx);

	int printMediaAllStreamDetail(const char* path);
}


class FFmpegPool {
public:
	/*					| reusable	|
	 * AVFormatContext	|			|
	 * AVCodec			|	Y		|
	 * AVCodecContext	|			|
	 * SwrContext		|	Y		|
	 * SwsContext		|	Y		|
	 * AVPacket			|	Y		|
	 * AVFrame			|	Y		|
	 * AVStream			|			|
	 */
	using CodecsPool = Token::ThreadMap<std::map, AVCodecID, const AVCodec*>;
	CodecsPool codecsPool;




	// 使用 pool 里有没有可用的 codec
	static void detectHaveReusable(const CodecsPool& codecsPool, AVCodecID codecId,
		bool& have_variable_codec, bool& have_allocated_codec, bool& have_reusable_codec
	) {
		have_variable_codec = codecsPool.tMap.contains(codecId);

		// have_allocated_codec
		if (have_variable_codec) {
			auto it = codecsPool.tMap.find(codecId);
			if (it != codecsPool.tMap.end()) have_allocated_codec = (it->second != nullptr);
			else have_allocated_codec = false;
		}

		// have_reusable_codec
		have_reusable_codec = (have_variable_codec && have_allocated_codec);
	}

	class ResourceGuard {
		using string = std::string;
		using shared_mutex = std::shared_mutex;
	public:
		// status 
		bool successful_acquired = false;
		string problem_description = "";

		// format context
		AVFormatContext* fmtCtx = nullptr;

		// codec
		std::shared_lock<shared_mutex> readLock_codec;
		const AVCodec* codecRef = nullptr;



		ResourceGuard(const char* path, CodecsPool& codecsPool, AVCodecID codecId) :
			readLock_codec(*codecsPool.tMtx, std::defer_lock)
		{
			// 不复用（一次性）创建
			FFmpegTool::AVFormatContext_open(fmtCtx, path);

			// 复用 创建

			// codec
			bool have_variable_codec;
			bool have_allocated_codec;
			bool have_reusable_codec;

			readLock_codec.lock();
			detectHaveReusable(codecsPool, codecId, have_variable_codec, have_allocated_codec, have_reusable_codec);

			if (!have_reusable_codec) {
				// 准备 codec
				const AVCodec* tempCodec = avcodec_find_decoder(codecId);
				if (!tempCodec) { problem_description += "[codec] avcodec_find_decoder can't found decoder (codec) \n"; }

				// 赋值 codec
				readLock_codec.unlock();
				codecsPool.tMtx->lock();
				if (!have_variable_codec) {
					codecsPool.tMap.insert_or_assign(codecId, tempCodec);
				}
				else if (!have_allocated_codec) {
					codecsPool.tMap[codecId] = tempCodec;
				}
				codecsPool.tMtx->unlock();
				readLock_codec.lock();
				detectHaveReusable(codecsPool, codecId, have_variable_codec, have_allocated_codec, have_reusable_codec);	// 重新上锁后的重新检测
			}

			successful_acquired = have_reusable_codec;
		}


		~ResourceGuard() {
			// 不复用（一次性）销毁
			FFmpegTool::AVFormatContext_close(fmtCtx);
		}
	};

	ResourceGuard getResource(const char* path, AVCodecID codecId) {
		return ResourceGuard(path, codecsPool, codecId);
	}
};

class Decoder {
public:

	// 内容器
	class FFmpegFormatContext :
		public Token::TokenAvailableImpl<FFmpegFormatContext>
	{
	public:
		AVFormatContext* fmtCtx = nullptr;

		// =================== consturctor ===================
		~FFmpegFormatContext() {
			if (fmtCtx) {
				avformat_close_input(&fmtCtx); // 会自动释放内部内存并置空
			}
		}

		FFmpegFormatContext() : TokenAvailableImpl(true), fmtCtx(nullptr) {}

		// 当有 path 传入时表示这个 fmtCtx 已经被占用（false）
		FFmpegFormatContext(const char* path) : TokenAvailableImpl(false) {
			if (setAVFormatContext(fmtCtx, path)) {}
			else {
				isAvailable.store(true); // 打开失败，不占用
			}
		}

		// =================== operator ===================

		// ❌ 禁止拷贝
		FFmpegFormatContext(const FFmpegFormatContext&) = delete;
		FFmpegFormatContext& operator=(const FFmpegFormatContext&) = delete;

		// ✅ 允许移动
		FFmpegFormatContext(FFmpegFormatContext&& other) noexcept {
			fmtCtx = other.fmtCtx;
			other.fmtCtx = nullptr;
			isAvailable.store(other.isAvailable.load());
		}

		FFmpegFormatContext& operator=(FFmpegFormatContext&& other) noexcept {
			if (this != &other) {
				if (fmtCtx) {
					avformat_close_input(&fmtCtx);
				}
				fmtCtx = other.fmtCtx;
				other.fmtCtx = nullptr;
				isAvailable.store(other.isAvailable.load());
			}
			return *this;
		}


		// ================= method =================
		// 检测当前是否可占用，可以则标记 "占用(false)"，并返回 true
		// 需要使用 releaseAfterUsed 才能重置回 "未占用（true）"
		bool tryUse(const char* path) {
			bool canUse = tryAcquire();
			if (canUse) {
				if (!setAVFormatContext(path)) {	// 可以使用但是源文件打不开
					releaseAfterUsed(); // 打开失败时自动释放
					return false;
				}
			}
			return canUse;
		}

		void releaseAfterUsed() {
			if (fmtCtx) {
				avformat_close_input(&fmtCtx);
				fmtCtx = nullptr;
			}
			isAvailable.store(true);
		}

		bool setAVFormatContext(const char* path) {
			return setAVFormatContext(fmtCtx, path);
		}

		static bool setAVFormatContext(AVFormatContext*& fmtCtx, const char* path) {
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
	};

	class FFmpegFormatContexts {
	public:
		std::vector<FFmpegFormatContext> fmtCtxs;
		std::mutex mtx;
		std::atomic<size_t> maximumAmt;

		explicit FFmpegFormatContexts(size_t maxAmt = 8) : maximumAmt(maxAmt) { fmtCtxs.reserve(maxAmt); }


		// 返回可用的内容器指针（FFmpegFormatContext），没有则 nullptr
		FFmpegFormatContext* useAvailableAVFormatContext(const char* path) {
			// 遍历寻找哪个可用
			{
				std::lock_guard<std::mutex> lock(mtx); // ✅ 加锁防止同时 emplace_back
				for (auto& ctx : fmtCtxs) {
					if (ctx.tryUse(path)) {
						return &ctx;
					}
				}

				// 遍历全部后没有可用的就检查 fmtCtxs 是否到达上限，没有则添加新的使用
				return addFmtCtx(path);
			}
		}

		template<typename... Args>
		FFmpegFormatContext* addFmtCtxImpl(Args&&... args) {
			if (fmtCtxs.size() >= maximumAmt) {
				std::cout << "The maximum amount (" << maximumAmt << ") of FFmpegFormatContext has been reached\n";
				return nullptr;
			}
			else {
				auto& newFmtCtx = fmtCtxs.emplace_back(std::forward<Args>(args)...);
				return &newFmtCtx;

			}
		}


		FFmpegFormatContext* addFmtCtx(const char* path) { return addFmtCtxImpl(path); }
		bool addFmtCtx() { return addFmtCtxImpl() != nullptr; }

		void releaseAll() {
			std::lock_guard<std::mutex> lock(mtx);
			for (auto& ctx : fmtCtxs) {
				ctx.releaseAfterUsed();
			}
		}

	};

	// 解码器
	class FFmpegCodecs {
	public:
		std::unordered_map<AVCodecID, const AVCodec*> AVCodecs;
		// LRU order (most-recent at front)
		std::list<AVCodecID> order;
		std::unordered_map<AVCodecID, std::list<AVCodecID>::iterator> orderMap;

		std::shared_mutex sharedMtx;
		std::atomic<size_t> maximumAmt;

		FFmpegCodecs(size_t maxAmt = 0) :maximumAmt(maxAmt) {}


		// 找可用的 codec，没有则返回 nullptr
		const AVCodec* getAVCodec(AVCodecID codecId) {
			const AVCodec* codec = nullptr;

			// ----------- Step 1: 读锁查找缓存 -----------
			{
				std::shared_lock readLock(sharedMtx);
				auto it = AVCodecs.find(codecId);
				if (it != AVCodecs.end()) {		// 找到了
					codec = it->second;
					// 如果不是最近使用的，准备移动
					if (order.front() != codecId) {
						// 升级为写锁更新 LRU
						readLock.unlock();
						std::unique_lock writeLock(sharedMtx);
						LRU(codecId);
					}
					return codec;
				}
			}

			// ----------- Step 2: 写锁查找并添加 -----------
			std::unique_lock writeLock(sharedMtx);

			// Double-check 防止 race
			auto it = AVCodecs.find(codecId);
			if (it != AVCodecs.end()) {
				LRU(codecId);
				return it->second;
			}

			// 超过缓存上限 -> 删除最旧项 
			if (AVCodecs.size() >= maximumAmt.load(std::memory_order_acquire) && maximumAmt != 0) {	// 容量 = 上限：标识容量已满，size != 0: 标识有上限
				AVCodecID oldId = order.back();
				order.pop_back();
				AVCodecs.erase(oldId);
				orderMap.erase(oldId);
			}

			// ----------- Step 3: 真正创建新的 decoder -----------
			const AVCodec* newCodec = avcodec_find_decoder(codecId);
			if (!newCodec) {
				std::cout << "Cannot find decoder for codecId: " << codecId << "\n";
				return nullptr;
			}

			auto [itNew, _] = AVCodecs.emplace(codecId, newCodec);
			order.emplace_front(codecId);
			orderMap[codecId] = order.begin();

			return itNew->second;
		}

	private:
		void LRU(AVCodecID codecId) {
			auto it = orderMap.find(codecId);
			if (it == orderMap.end()) return;
			auto listIt = it->second;
			if (listIt != order.begin()) {
				order.splice(order.begin(), order, listIt);
				it->second = order.begin(); // 更新迭代器
			}
		}

	public:
		// 设置最大缓存量（线程安全）
		void setMaximum(size_t maxAmt) {
			maximumAmt.store(maxAmt, std::memory_order_release);
		}

		size_t getMaximum() const {
			return maximumAmt.load(std::memory_order_acquire);
		}

		// 清空缓存（线程安全）
		void clear() {
			std::unique_lock writeLock(sharedMtx);
			AVCodecs.clear();
			order.clear();
			orderMap.clear();
		}
	};

	// 解码环境（解码工具间）
	class FFmpegCodecContext :
		public Token::TokenAvailableImpl<FFmpegCodecContext> {
	public:
		AVCodecContext* codecCtx = nullptr;

		// =================== consturctor ===================
		~FFmpegCodecContext() {
			if (codecCtx) {
				avcodec_free_context(&codecCtx); // 会自动释放内部内存并置空
			}
		}

		FFmpegCodecContext() :TokenAvailableImpl<FFmpegCodecContext>(true), codecCtx(nullptr) {}

		// 仅传入解码器 (codec), 仅设置 codecCtx 的类型，仍空闲
		FFmpegCodecContext(AVStream* stream, const AVCodec* codec) :
			TokenAvailableImpl<FFmpegCodecContext>(true)
		{
			setAVCodecContext(stream, codec, codecCtx);
		}


		// =================== operator ===================
		// ❌ 禁止拷贝
		FFmpegCodecContext(const FFmpegCodecContext&) = delete;
		FFmpegCodecContext& operator=(const FFmpegCodecContext&) = delete;

		// ✅ 允许移动
		FFmpegCodecContext(FFmpegCodecContext&& other) noexcept {
			codecCtx = other.codecCtx;
			other.codecCtx = nullptr;
			isAvailable.store(other.isAvailable.load());
		}

		FFmpegCodecContext& operator=(FFmpegCodecContext&& other) noexcept {
			if (this != &other) {
				if (codecCtx) {
					avcodec_free_context(&codecCtx); // 会自动释放内部内存并置空
				}
				codecCtx = other.codecCtx;
				other.codecCtx = nullptr;
				isAvailable.store(other.isAvailable.load());
			}
			return *this;
		}


		// ================= method =================
		// 检测当前是否可占用，可以则标记 "占用(false)"，并返回 true
		// 需要使用 releaseAfterUsed 才能重置回 "未占用（true）"


		bool tryPerfectUse(AVStream* stream, const AVCodec*& codec) {
			if (!tryAcquire()) return false;
			// ( codecCtx 有值 + codec 类型匹配 + stream 配置匹配 )
			// ( Y codecCtx & Y codec & Y Stream ): flush_buffers
			// ( Y codecCtx & Y codec & N Stream ): re-alloc_context3
			// ( Y codecCtx & N codec & Y Stream ): re-alloc_context3
			// ( Y codecCtx & N codec & N Stream ): re-alloc_context3
			// 
			// ( N codecCtx & Y codec & Y Stream ): alloc_context3 
			// ( N codecCtx & Y codec & N Stream ): alloc_context3 
			// ( N codecCtx & N codec & Y Stream ): alloc_context3 
			// ( N codecCtx & N codec & N Stream ): alloc_context3 
			// 这里只取 (YYY) 和 (N__ + 赋值成功) 返回 true

			if (!codecCtx) {
				// 没有 context → 新建
				if (setAVCodecContext(stream, codec, codecCtx)) {
					return true;
				}
				else {
					releaseAfterUsed();
					return false;
				}
			}
			else if (isCodecContextReusable(stream, codec, codecCtx)) {
				avcodec_flush_buffers(codecCtx);
				return true;
			}
			else {
				releaseAfterUsed();
				return false;
			}
		}

		bool tryUse(AVStream* stream, const AVCodec*& codec) {
			if (!tryAcquire()) return false;
			// 这里取 codecCtx 以任何方式赋值成功

			if (!codecCtx) {
				// 没有 context → 新建
				if (setAVCodecContext(stream, codec, codecCtx)) {
					return true;
				}
				else {
					releaseAfterUsed();
					return false;
				}
			}
			else if (isCodecContextReusable(stream, codec, codecCtx)) {
				avcodec_flush_buffers(codecCtx);
				return true;
			}
			else {
				avcodec_free_context(&codecCtx);
				if (setAVCodecContext(stream, codec, codecCtx)) {
					return true;
				}
				else {
					releaseAfterUsed();
					return false;
				}
			}


		}

		void releaseAfterUsed() {
			if (codecCtx) {
				avcodec_flush_buffers(codecCtx);
			}
			isAvailable.store(true);
		}

		static bool setAVCodecContext(AVStream* stream, const AVCodec*& codec, AVCodecContext*& codecCtx) {

			if (codecCtx) {
				avcodec_free_context(&codecCtx);
			}

			codecCtx = avcodec_alloc_context3(codec);
			if (!codecCtx) { return false; }

			// 在工具间里放入需要的资料
			if (avcodec_parameters_to_context(codecCtx, stream->codecpar) < 0) {
				std::cout << "Failed to copy codec parameters to context \n"; return false;
			}

			// 在工具间里初始化解码实例
			if (avcodec_open2(codecCtx, codec, nullptr) < 0) {
				std::cout << "Failed to open decoder\n"; return false;
			}

			return true;
		}

		static bool isCodecContextReusable(AVStream* stream, const AVCodec* codec, AVCodecContext* codecCtx) {
			if (!codec || !stream || !stream->codecpar || !codecCtx)
				return false;

			AVCodecParameters* p = stream->codecpar;

			// codec_id 必须一致
			if (codecCtx->codec_id != codec->id)
				return false;

			if (codec->id != p->codec_id)
				return false;

			if (p->sample_rate <= 0 || p->format < 0)
				return false;

			return true;
		}



	};

	class FFmpegCodecContexts {
	public:
		std::vector<FFmpegCodecContext> codecCtxs;
		std::mutex mtx;
		std::atomic<size_t> maximumAmt = SIZE_MAX;

		explicit FFmpegCodecContexts(size_t maxAmt = 5) : maximumAmt(maxAmt) { codecCtxs.reserve(maxAmt); }


		// 返回可用的解码环境指针（FFmpegCodecContext），没有则 nullptr
		FFmpegCodecContext* useAvailableAVFormatContext(AVStream* stream, const AVCodec* codec) {
			// 遍历寻找哪个可用
			{
				std::lock_guard<std::mutex> lock(mtx); // ✅ 加锁防止同时 emplace_back
				for (auto& codecCtx : codecCtxs) {
					if (codecCtx.tryPerfectUse(stream, codec)) {
						return &codecCtx;
					}
				}

				for (auto& codecCtx : codecCtxs) {
					if (codecCtx.tryUse(stream, codec)) {
						return &codecCtx;
					}
				}
				// 遍历全部后没有可用的就检查 codecCtxs 是否到达上限，没有则添加新的使用
				return addcodecCtx(stream, codec);
			}
		}

		// 在不超过 maximum 的情况下添加 codecCtx, 添加失败返回 nullptr
		FFmpegCodecContext* addcodecCtx(AVStream* stream, const AVCodec* codec) {
			if (maximumAmt == 0 || codecCtxs.size() < maximumAmt) {
				auto& newCodecCtx = codecCtxs.emplace_back(stream, codec);
				return &newCodecCtx;
			}
			else {
				std::cout << "The maximum amount (" << maximumAmt << ") of FFmpegFormatContext has been reached\n";
				return nullptr;
			}
		}

		void releaseAll() {
			std::lock_guard<std::mutex> lock(mtx);
			for (auto& codecCtx : codecCtxs) {
				codecCtx.releaseAfterUsed();
			}
		}
	};

	// 播放设置磨合器
	class FFmpegSwrContext {
	public:
		// 使用规范
		// 使用结束清理不可复用的部分，以便复用时不用在检测环节重复清理
		// 使用时优先搜索已配置好的工具，没有则尝试生成一个新的，如果达到上限则选择初始化一个待机的工具

		std::atomic<bool> isAvailable = true;
		SwrContext* swrCtx = nullptr;

		// 缓存上次参数，判断复用条件
		int in_sample_rate = 0;
		int out_sample_rate = 0;
		AVSampleFormat in_fmt = AV_SAMPLE_FMT_NONE;
		AVSampleFormat out_fmt = AV_SAMPLE_FMT_NONE;
		AVChannelLayout in_ch_layout;
		AVChannelLayout out_ch_layout;

		// =================== 构造与析构 ===================
		~FFmpegSwrContext() {
			free();
		}

		FFmpegSwrContext() = default;

		FFmpegSwrContext(
			AVCodecContext* codecCtx,
			int target_rate,
			AVSampleFormat target_fmt = AV_SAMPLE_FMT_S16,	// out
			const AVChannelLayout* out_layout = nullptr		// out
		) : isAvailable(true) {
			setSwrContext(codecCtx, target_rate, target_fmt, out_layout);

		}

		// 禁止复制
		FFmpegSwrContext(const FFmpegSwrContext&) = delete;
		FFmpegSwrContext& operator=(const FFmpegSwrContext&) = delete;

		// 允许移动
		FFmpegSwrContext(FFmpegSwrContext&& other) noexcept {
			moveFrom(std::move(other));
		}

		FFmpegSwrContext& operator=(FFmpegSwrContext&& other) noexcept {
			if (this != &other) {
				free();
				moveFrom(std::move(other));
			}
			return *this;
		}

		// =================== 基础操作 ===================
		bool tryUse() {
			bool expected = true;
			return isAvailable.compare_exchange_strong(expected, false);
		}

		void releaseAfterUsed() {
			if (swrCtx) {
				// 复用，清空缓存
				swr_convert(swrCtx, nullptr, 0, nullptr, 0);
			}
			isAvailable.store(true);
		}

		void free() {
			if (swrCtx) {
				swr_free(&swrCtx);
			}
			if (in_ch_layout.nb_channels > 0)
				av_channel_layout_uninit(&in_ch_layout);
			if (out_ch_layout.nb_channels > 0)
				av_channel_layout_uninit(&out_ch_layout);
			in_fmt = out_fmt = AV_SAMPLE_FMT_NONE;
			in_sample_rate = out_sample_rate = 0;
		}

		bool isInitialized() const noexcept { return swrCtx != nullptr; }
		// =================== 初始化与复用 ===================
		bool tryPerfectUse(
			AVCodecContext* codecCtx, int target_rate,
			AVSampleFormat target_fmt = AV_SAMPLE_FMT_S16,
			const AVChannelLayout* out_layout = nullptr
		) {
			if (!tryUse()) return false;

			if (!codecCtx || codecCtx->sample_rate <= 0 || codecCtx->ch_layout.nb_channels <= 0) {
				releaseAfterUsed();
				return false;
			}
			// (swrCtc 是否有值 & 能否复用)
			// ( Y & Y ): swr_convert
			// ( Y & N ): swr_alloc_set_opts2
			// ( N & _ ): swr_alloc_set_opts2


			if (!swrCtx) {
				if (setSwrContext(codecCtx, target_rate, target_fmt, out_layout)) {
					return true;
				}
				else {
					releaseAfterUsed();
					return false;
				}
			}
			else if (isReusable(codecCtx, target_rate, target_fmt, out_layout)) {
				// 参数相同 → 复用，清空缓存
				swr_convert(swrCtx, nullptr, 0, nullptr, 0);
				return true;
			}
			else {

				releaseAfterUsed();
				return false;

			}
		}

		bool tryUse(AVCodecContext* codecCtx, int target_rate,
			AVSampleFormat target_fmt = AV_SAMPLE_FMT_S16,
			const AVChannelLayout* out_layout = nullptr
		) {
			if (!tryUse()) return false;

			if (!codecCtx || codecCtx->sample_rate <= 0 || codecCtx->ch_layout.nb_channels <= 0) {
				releaseAfterUsed();
				return false;
			}

			if (!swrCtx) {
				if (setSwrContext(codecCtx, target_rate, target_fmt, out_layout)) {
					return true;
				}
				else {
					releaseAfterUsed();
					return false;
				}
			}
			else if (isReusable(codecCtx, target_rate, target_fmt, out_layout)) {
				// 参数相同 → 复用，清空缓存
				swr_convert(swrCtx, nullptr, 0, nullptr, 0);
				return true;
			}
			else {
				// 参数不同 → 重新分配
				free();
				if (setSwrContext(codecCtx, target_rate, target_fmt)) {
					return true;
				}
				else {
					releaseAfterUsed();
					return false;
				}
			}
		}



	private:
		// 检测是否可复用
		bool isReusable(
			AVCodecContext* codecCtx, int target_rate,
			AVSampleFormat target_fmt,
			const AVChannelLayout* customOutLayout = nullptr
		) const {
			if (!swrCtx || !codecCtx) return false;

			if (in_sample_rate != codecCtx->sample_rate ||
				out_sample_rate != target_rate ||
				in_fmt != codecCtx->sample_fmt ||
				out_fmt != target_fmt)
				return false;

			// 比较声道布局
			if (av_channel_layout_compare(&in_ch_layout, &codecCtx->ch_layout) != 0)
				return false;
			if (customOutLayout) {
				if (av_channel_layout_compare(&out_ch_layout, customOutLayout) != 0)
					return false;
			}
			else {
				if (av_channel_layout_compare(&out_ch_layout, &codecCtx->ch_layout) != 0)
					return false;
			}

			return true;
		}

		// 内部使用：设置参数缓存
		// 成员版：创建并缓存参数（负责设置 in/out layout 与缓存）
		bool setSwrContext(AVCodecContext* codecCtx,
			int out_rate,
			AVSampleFormat out_fmt,
			const AVChannelLayout* customOutLayout = nullptr) {
			if (!codecCtx) return false;

			// 先重置旧的 layout（如果有）
			if (in_ch_layout.nb_channels > 0) av_channel_layout_uninit(&in_ch_layout);
			if (out_ch_layout.nb_channels > 0) av_channel_layout_uninit(&out_ch_layout);

			// 复制输入 layout（来自 codecCtx）
			if (av_channel_layout_copy(&in_ch_layout, &codecCtx->ch_layout) < 0) return false;

			// 设置输出 layout（使用传入的 customOutLayout 或 codecCtx 的 layout）
			if (customOutLayout) {
				if (av_channel_layout_copy(&out_ch_layout, customOutLayout) < 0) return false;
			}
			else {
				if (av_channel_layout_copy(&out_ch_layout, &codecCtx->ch_layout) < 0) return false;
			}

			int ret = swr_alloc_set_opts2(
				&swrCtx,
				&out_ch_layout,          // 输出布局
				out_fmt,                 // 输出格式
				out_rate,                // 输出采样率
				&in_ch_layout,           // 输入布局
				codecCtx->sample_fmt,    // 输入采样格式
				codecCtx->sample_rate,   // 输入采样率
				0,
				nullptr
			);
			if (ret < 0) {
				// 若 swrCtx 被分配失败，应确保没有残留
				if (swrCtx) { swr_free(&swrCtx); swrCtx = nullptr; }
				return false;
			}
			if (swr_init(swrCtx) < 0) {
				swr_free(&swrCtx);
				return false;
			}

			// 缓存参数以便后续复用判定
			in_sample_rate = codecCtx->sample_rate;
			out_sample_rate = out_rate;
			in_fmt = codecCtx->sample_fmt;
			out_fmt = out_fmt;

			return true;
		}

		void moveFrom(FFmpegSwrContext&& other) {
			swrCtx = other.swrCtx;
			other.swrCtx = nullptr;

			in_sample_rate = other.in_sample_rate;
			out_sample_rate = other.out_sample_rate;
			in_fmt = other.in_fmt;
			out_fmt = other.out_fmt;
			// copy layouts only if they are valid
			if (other.in_ch_layout.nb_channels > 0) {
				av_channel_layout_copy(&in_ch_layout, &other.in_ch_layout);
				av_channel_layout_uninit(&other.in_ch_layout);
			}
			else {
				av_channel_layout_uninit(&in_ch_layout);
			}
			if (other.out_ch_layout.nb_channels > 0) {
				av_channel_layout_copy(&out_ch_layout, &other.out_ch_layout);
				av_channel_layout_uninit(&other.out_ch_layout);
			}
			else {
				av_channel_layout_uninit(&out_ch_layout);
			}
			isAvailable.store(other.isAvailable.load());
		}
	};

	class FFmpegSwrContexts {
	public:
		std::vector<FFmpegSwrContext> swrCtxs;
		std::mutex mtx;
		std::atomic<size_t> maximumAmt = SIZE_MAX;



		explicit FFmpegSwrContexts(size_t maxCount = 8)
			: maximumAmt(maxCount) {
			swrCtxs.reserve(maxCount);
		}

		FFmpegSwrContext* useAvailableSwrContext(
			AVCodecContext* codecCtx,
			int target_rate,
			AVSampleFormat target_fmt = AV_SAMPLE_FMT_S16,
			const AVChannelLayout* out_layout = nullptr
		) {
			std::lock_guard<std::mutex> lock(mtx);

			// 优先查找可用的
			for (auto& ctx : swrCtxs) {
				if (ctx.tryPerfectUse(codecCtx, target_rate, target_fmt, out_layout))
					return &ctx;
			}

			for (auto& swrCtx : swrCtxs) {
				if (swrCtx.tryUse(codecCtx, target_rate, target_fmt, out_layout)) {
					return &swrCtx;
				}
			}

			// 没找到空闲可复用的 + 没有找到空闲的 → 如果没达到上限，就创建一个新的
			return addSwrCtx(codecCtx, target_rate, target_fmt, out_layout); // 没找到、也无法新建
		}

		// 在不超过 maximum 的情况下添加 codecCtx, 添加失败返回 nullptr
		FFmpegSwrContext* addSwrCtx(
			AVCodecContext* codecCtx,
			int target_rate,
			AVSampleFormat target_fmt = AV_SAMPLE_FMT_S16,
			const AVChannelLayout* out_layout = nullptr
		) {
			if (swrCtxs.size() >= maximumAmt) {
				std::cerr << "[FFmpegSwrContexts] Reached maximum count: " << maximumAmt << "\n";
				return nullptr;
			}

			try {
				swrCtxs.emplace_back(codecCtx, target_rate, target_fmt, out_layout);
				auto& ctx = swrCtxs.back();
				if (ctx.isInitialized())
					return &ctx;
				swrCtxs.pop_back();
			}
			catch (...) {
				std::cerr << "[FFmpegSwrContexts] Exception during addSwrCtx()\n";
			}
			return nullptr;

		}
		// 完全清除
		void clear() {
			std::lock_guard<std::mutex> lock(mtx);
			swrCtxs.clear();
		}

		// 解放所有正在任务的磨合器
		void releaseAll() {
			std::lock_guard<std::mutex> lock(mtx);
			for (auto& codecCtx : swrCtxs) {
				codecCtx.releaseAfterUsed();
			}
		}


		size_t size() const noexcept { return swrCtxs.size(); }

		size_t capacity() const noexcept { return maximumAmt.load(); }

		void setMax(size_t max) noexcept { maximumAmt.store(max); }
	};

	// 包
	class FFmpegPacket {
	public:
		// packet status		: if reuse			: if del
		// ( 有指针 & 有数据 )	
		// ( 指针 Y & 数据 Y )	: av_packet_unref	: unref + free
		// ( 指针 Y & 数据 N )	: -					: free
		// ( 指针 N & 数据 _ )	: av_packet_alloc	: -
		// 
		// del:
		// av_packet_unref(packet)	: 清理内部数据（重新可用）
		// av_packet_free(&packet)	: 释放结构体内存（不可再用）
		//
		AVPacket* packet = nullptr;				// class 的主体
		std::atomic<bool> isAvailable = true;	// 通过访问 isAvailable 确认 packet 是否被占用（避免在多线程时大量数据同时访问 packet 资源抢占）

		// =================== 构造与析构 ===================

		// 构造一个可以直接使用的 packet
		FFmpegPacket() {
			packet = av_packet_alloc();
		}

		// 完全清除干净
		~FFmpegPacket() {
			free();
		}

		// 禁止复制
		FFmpegPacket(const FFmpegPacket&) = delete;
		FFmpegPacket& operator=(const FFmpegPacket&) = delete;

		// 允许移动
		FFmpegPacket(FFmpegPacket&& other) noexcept {
			moveFrom(std::move(other));
		}

		FFmpegPacket& operator=(FFmpegPacket&& other) noexcept {
			if (this != &other) {
				free();
				moveFrom(std::move(other));
			}
			return *this;
		}

		// =================== 可用性控制 ===================

		// 尝试调用，成功则返回 true
		bool tryUse() {
			bool expected = true;
			return isAvailable.compare_exchange_strong(expected, false);
		}

		void releaseAfterUsed() {
			if (packet)
				av_packet_unref(packet); // 清空内容，保留结构体
			isAvailable.store(true);
		}

		bool isInitialized() const noexcept {
			return packet != nullptr;
		}

		// =================== 操作函数 ===================
		void reset() {
			if (packet)
				av_packet_unref(packet);
		}

		void free() {
			if (packet) {
				av_packet_free(&packet);
				packet = nullptr;
			}
			isAvailable.store(true);
		}

	private:
		void moveFrom(FFmpegPacket&& other) {
			packet = other.packet;
			other.packet = nullptr;

			isAvailable.store(other.isAvailable.load());
			other.isAvailable.store(true); // 旧对象重置为空闲
		}
	};

	class FFmpegPackets {
	public:
		std::vector<FFmpegPacket> packets;
		std::mutex mtx;
		std::atomic<size_t> maximumAmt = SIZE_MAX;


		FFmpegPackets(size_t maxCount = 8)
			: maximumAmt(maxCount) {
			packets.reserve(maxCount);
		}

		// 获取一个可用的 packet，复用或创建
		FFmpegPacket* useAvailablePacket() {
			std::lock_guard<std::mutex> lock(mtx);

			// 优先查找空闲可复用的 packet
			for (auto& pkt : packets) {
				if (pkt.tryUse())
					return &pkt;
			}

			// 没找到，尝试创建新的（不超过最大值）
			return addPacket();
		}

		// 添加一个新的 packet（初始化），如果达到上限返回 nullptr
		FFmpegPacket* addPacket() {
			if (packets.size() >= maximumAmt) {
				std::cerr << "[FFmpegPackets] Reached maximum count: " << maximumAmt << "\n";
				return nullptr;
			}

			try {
				packets.emplace_back(); // 调用默认构造
				auto& pkt = packets.back();
				if (pkt.isInitialized())
					return &pkt;
				packets.pop_back(); // 初始化失败回滚
			}
			catch (...) {
				std::cerr << "[FFmpegPackets] Exception during addPacket()\n";
			}

			return nullptr;
		}

		// 释放所有 packet 的内容（保留结构体，可复用）
		void releaseAll() {
			std::lock_guard<std::mutex> lock(mtx);
			for (auto& pkt : packets) {
				pkt.releaseAfterUsed();
			}
		}

		// 清除所有结构体（完全释放）
		void clear() {
			std::lock_guard<std::mutex> lock(mtx);
			packets.clear();
		}

		size_t size() const noexcept {
			return packets.size();
		}

		size_t capacity() const noexcept {
			return maximumAmt.load();
		}

		void setMax(size_t max) noexcept {
			maximumAmt.store(max);
		}
	};

	// 帧
	class FFmpegFrame {
	public:
		AVFrame* frame = av_frame_alloc();
		std::atomic<bool> isAvailable = true;	// 通过访问 isAvailable 确认 frame 是否被占用（避免在多线程时大量数据同时访问 frame 资源抢占）

		// =================== 构造与析构 ===================
		explicit FFmpegFrame() {
			frame = av_frame_alloc();
		}

		~FFmpegFrame() {
			free();
		}

		// 禁止复制
		FFmpegFrame(const FFmpegFrame&) = delete;
		FFmpegFrame& operator=(const FFmpegFrame&) = delete;

		// 允许移动
		FFmpegFrame(FFmpegFrame&& other) noexcept {
			moveFrom(std::move(other));
		}

		FFmpegFrame& operator=(FFmpegFrame&& other) noexcept {
			if (this != &other) {
				free();
				moveFrom(std::move(other));
			}
			return *this;
		}

		// =================== 可用性控制 ===================
		bool tryUse() {
			bool expected = true;
			return isAvailable.compare_exchange_strong(expected, false);
		}

		void releaseAfterUsed() {
			if (frame)
				av_frame_unref(frame); // 清理内部数据，保留结构体
			isAvailable.store(true);
		}

		bool isInitialized() const noexcept {
			return frame != nullptr;
		}

		// =================== 操作函数 ===================
		void reset() {
			if (frame)
				av_frame_unref(frame);
		}

		void free() {
			if (frame) {
				av_frame_free(&frame);
				frame = nullptr;
			}
			isAvailable.store(true);
		}

	private:
		void moveFrom(FFmpegFrame&& other) {
			frame = other.frame;
			other.frame = nullptr;
			isAvailable.store(other.isAvailable.load());
			other.isAvailable.store(true);
		}
	};

	class FFmpegFrames {
	public:
		std::vector<FFmpegFrame> frames;
		std::mutex mtx;
		std::atomic<size_t> maximumAmt = SIZE_MAX;

		explicit FFmpegFrames(size_t maxCount = 8)
			: maximumAmt(maxCount) {
			frames.reserve(maxCount);
		}

		// 获取一个可用的 frame（优先复用，否则新建）
		FFmpegFrame* useAvailableFrame() {
			std::lock_guard<std::mutex> lock(mtx);

			for (auto& frm : frames) {
				if (frm.tryUse())
					return &frm;
			}

			return addFrame();
		}

		// 添加一个新 frame（不超过最大数）
		FFmpegFrame* addFrame() {
			if (frames.size() >= maximumAmt) {
				std::cerr << "[FFmpegFrames] Reached maximum count: " << maximumAmt << "\n";
				return nullptr;
			}

			try {
				frames.emplace_back();
				auto& frm = frames.back();
				if (frm.isInitialized())
					return &frm;
				frames.pop_back();
			}
			catch (...) {
				std::cerr << "[FFmpegFrames] Exception during addFrame()\n";
			}
			return nullptr;
		}

		// 清空所有 Frame 内容（保留结构体）
		void releaseAll() {
			std::lock_guard<std::mutex> lock(mtx);
			for (auto& frm : frames)
				frm.releaseAfterUsed();
		}

		// 彻底删除所有 Frame（释放结构体内存）
		void clear() {
			std::lock_guard<std::mutex> lock(mtx);
			frames.clear();
		}

		size_t size() const noexcept { return frames.size(); }
		size_t capacity() const noexcept { return maximumAmt.load(); }
		void setMax(size_t max) noexcept { maximumAmt.store(max); }
	};

	class DecodeMission {
		// 单个解码任务的所需变量
		// 是否解码完毕		- isCompleteDecode	- atomic<bool> (已多线程安全, 读写不需要额外锁)
		// 是否允许进行解码	- isAllowDecode		- atomic<bool> (已多线程安全, 读写不需要额外锁)
		// 解码后的数据		- data
		//
		using string = std::string;
		using shared_mutex = std::shared_mutex;
		using shared_lock = std::shared_lock<std::shared_mutex>;

	public:
		Token::TokenAvailable isAllowDecode;					// 是否允许对该媒体文件进行解码
		Token::TokenAvailable isCompleteDecode;					// 是否解码完一个媒体文件的所有内容
		Token::ThreadVector<Token::ThreadVector<uint8_t>> data;	// data[秒] 

		// ================= constructor =================

		DecodeMission() : isAllowDecode(true), isCompleteDecode() {}

		DecodeMission(int size) : isAllowDecode(true), isCompleteDecode(), data(size) {}

		// ================= operator =================

		DecodeMission(const DecodeMission&) = default;
		DecodeMission& operator=(const DecodeMission&) = default;

		DecodeMission(DecodeMission&&) noexcept = default;
		DecodeMission& operator=(DecodeMission&&) noexcept = default;

		// ================= method =================

		// getter
		bool getIsAllowDecode() { return isAllowDecode.getIsAvailable(); }
		bool getIsCompleteDecode() { return isCompleteDecode.getIsAvailable(); }


		bool isAllDataDecodeFinish() {
			{
				auto guard = data.read_lock();
				for (auto& sec_data : *guard) {
					if (sec_data.empty()) { return false; }
				}
			}
			return true;
		}


	};

	using DecodeMissionMap = Token::ThreadMap<std::unordered_map, std::string, DecodeMission>;

	class DecodeMissions {
		using string = std::string;
		using shared_mutex = std::shared_mutex;
		using shared_lock = std::shared_lock<std::shared_mutex>;
		using unique_lock = std::unique_lock<std::shared_mutex>;
	public:

		Token::ThreadMap<std::unordered_map, string, DecodeMission> decodeMissions;
		// ================= constructor =================

		DecodeMissions() = default;

		// ================= operator =================

		// ================= method =================

		// getter

		bool getIsCompleteDecode(string& str) {
			return decodeMissions.tMap[str].isCompleteDecode.getIsAvailable();	// atomic
		}

		bool getIsAllowDecode(string& str) {
			return decodeMissions.tMap[str].isAllowDecode.getIsAvailable();
		}

		// setter

		void setIsAllowDecode(string& str, bool exptected) {
			decodeMissions.tMap[str].isAllowDecode.setIsAvailable(exptected);
		}

		void setIsCompleteDecode(string& str) {
			decodeMissions
				.tMap[str]
				.isCompleteDecode
				.setIsAvailable(
					decodeMissions.tMap[str].isAllDataDecodeFinish()
				);
		}

		// 没有就添加；有就不作为
		// return decodeMission
		DecodeMission& addDecodeMission(const string& pathStr) {
			return decodeMissions.insert_or_assign(pathStr, {});
		}

	};
	/*
	 * std::vector<std::vector<uint8_t>> result // 储存每秒的PCM 数据
	 * 内存占用		: 把 capacity 设置为媒体文件的 duration , 这样在对未该秒数进行数据填充(vector<uint8_t>) 时就不会占用空间
	 * 帧跨秒的情况	: 取帧的其实位置为基准，t = 0.999s 也就是属于 0 秒的
	 *
	 */
	 //======================================================

	FFmpegFormatContexts fmtCtxs;	// AVFormatContext* fmtCtx = nullptr
	FFmpegCodecs codecs;			// const AVCodec* codec = nullptr;	

	FFmpegCodecContexts codecCtxs;	//AVCodecContext* codecCtx = nullptr
	FFmpegSwrContexts swrCtxs;

	FFmpegPackets packets;
	FFmpegFrames frames;

	DecodeMissions decodeMissions;
	DecodeMissionMap decodeMissionMap;

	Decoder() = default;


	void launch(const char* path) {
		// 1. 负责准备解码工具 (here)
		// 2，负责进行解码
		// 3. 解码的子环节
		//
		// 这个环节的步骤 (负责准备解码工具 (here))：
		// 1. (设置) 准备音源和设备的配置
		// 2. (工具集) 准备解码的工具 (FFmpeg)
		// 3. (数据库) 准备解码的任务, 有则使用;没有则创建. 
		// 4. () 将刚刚准备好的工具投入进行解码
		//


		// 以 SDL 的方式获得设备配置
		SDL_AudioSpec deviceAudioSpec;
		SDL_AudioSpec spec;
		spec.freq = 44100;
		spec.format = AUDIO_S16SYS;
		spec.channels = 2;
		spec.samples = 1024;
		spec.callback = nullptr;
		spec.userdata = nullptr;

		auto d = SDL_OpenAudioDevice(nullptr, 0, &spec, &deviceAudioSpec, 0);

		AVChannelLayout out_layout;
		SDLChannelsToAV(deviceAudioSpec.channels, &out_layout);



		// 先获得 FFmpeg 的 components

		auto* fmtCtx = fmtCtxs.useAvailableAVFormatContext(path);

		int audioStreamIndex = av_find_best_stream(fmtCtx->fmtCtx, AVMEDIA_TYPE_AUDIO, -1, -1, nullptr, 0);

		auto* codec = codecs.getAVCodec(fmtCtx->fmtCtx->streams[audioStreamIndex]->codecpar->codec_id);

		auto* codecCtx = codecCtxs.useAvailableAVFormatContext(fmtCtx->fmtCtx->streams[audioStreamIndex], codec);

		auto* swrCtx = swrCtxs.useAvailableSwrContext(codecCtx->codecCtx, deviceAudioSpec.freq, SDLFormatToAV(deviceAudioSpec.format), &out_layout);

		auto* packet = packets.useAvailablePacket();

		auto* frame = frames.useAvailableFrame();


		// 为 result 提前扩容后对 result 状态标记

		std::string pathStr = Conversion::toUTF8String(path);
		double durationSec = std::ceil(FFmpegTool::getStreamDurationSec(fmtCtx->fmtCtx, fmtCtx->fmtCtx->streams[audioStreamIndex]));

		auto& dm = decodeMissionMap.insert_or_assign(pathStr, DecodeMission(durationSec));




		// 正式进入解码
		FFmpegDecode_2(pathStr, dm, fmtCtx->fmtCtx, audioStreamIndex, codecCtx->codecCtx, swrCtx->swrCtx, packet->packet, frame->frame, deviceAudioSpec, 0);


		// 结束后清理环节
		fmtCtx->releaseAfterUsed();
		codecCtx->releaseAfterUsed();
		swrCtx->releaseAfterUsed();
		packet->releaseAfterUsed();
		frame->releaseAfterUsed();

		SDL_CloseAudioDevice(d);
		av_channel_layout_uninit(&out_layout);
	}

private:

	// origin
	void FFmpegDecode_2(
		std::string path,
		DecodeMission& dm,
		AVFormatContext* fmtCtx,
		int targetStreamIndex,
		AVCodecContext* codecCtx,
		SwrContext* swrCtx,
		AVPacket* packet,
		AVFrame* frame,
		int out_channels,
		AVSampleFormat out_format,
		double startTimeSec,
		int tempBufferSize = 192000
	) {
		/*
		 * 这秒解码过了没有 & 这秒的帧解码完了没有 & 是否允许解码 &
		 *
		 * Y & Y & Y = 切换下一帧
		 * Y & Y & N = 终止
		 * Y & N & Y = 切换下一帧
		 * Y & N & N = 终止
		 *
		 * N & Y & Y = 解码
		 * N & Y & N = 终止
		 * N & N & Y = 解码
		 * N & N & N = 解码
		 *
		 * simplified:
		 *
		 * Y & _ & Y = 切换下一帧
		 * Y & _ & N = 终止
		 *
		 * N & Y & Y = 解码
		 * N & Y & N = 终止
		 * N & N & _ = 解码
		 *
		 */


		int currentSec = static_cast<int>(startTimeSec);

		int64_t seekTarget = llround(startTimeSec / av_q2d(fmtCtx->streams[targetStreamIndex]->time_base));

		if (av_seek_frame(fmtCtx, targetStreamIndex, seekTarget, AVSEEK_FLAG_BACKWARD) < 0) {
			// Seek失败，可处理错误
		}

		avcodec_flush_buffers(codecCtx);

		std::vector<uint8_t> tempBuffer(tempBufferSize);

		// 预计算 bytesPerSample 用于 later safe maxOutSamples
		const int bytesPerSample = av_get_bytes_per_sample(out_format) > 0 ? av_get_bytes_per_sample(out_format) : 1;
		const int maxOutSamplesFromBuffer = tempBufferSize / (out_channels * bytesPerSample);


		while (av_read_frame(fmtCtx, packet) >= 0) {
			if (packet->stream_index == targetStreamIndex) {
				if (avcodec_send_packet(codecCtx, packet) == 0) {
					while (avcodec_receive_frame(codecCtx, frame) == 0) {
						int sec = getFrameSecond(frame, fmtCtx, targetStreamIndex);	// 这个帧的时间线
						// 那一秒的 vector 的 size (用于检测是不是空)
						int len = static_cast<int>(decodeMissionMap.getValueRef(path).data.at(sec).size());


						if (len != 0) {	// 不是 0 , 表示已经有数据填充
							if (dm.getIsAllowDecode()) {
								continue;	// skip 至下一帧
							}
							else {
								// 终止
								return;
							}
						}
						else {
							if (sec == currentSec) {	// 仍属于当前秒, 表示仍未解码完
								// 解码
								step_01(swrCtx, frame, out_channels, out_format, decodeMissionMap.getValueRef(path).data.at(sec).unsafe_ref(), *decodeMissionMap.getValueRef(path).data.at(sec).tMtx, tempBuffer, maxOutSamplesFromBuffer);
							}
							else {	// 不属于当前秒, 表示已解码完
								if (dm.getIsAllowDecode()) {	// 解码
									currentSec = sec;
									step_01(swrCtx, frame, out_channels, out_format, decodeMissionMap.getValueRef(path).data.at(sec).unsafe_ref(), *decodeMissionMap.getValueRef(path).data.at(sec).tMtx, tempBuffer, maxOutSamplesFromBuffer);
								}
								else {	// 终止
									return;
								}
							}
						}
					}
				}
			}
			av_packet_unref(packet);
		}

		// drain decoder
		avcodec_send_packet(codecCtx, nullptr);
		while (avcodec_receive_frame(codecCtx, frame) == 0) {
			int sec = getFrameSecond(frame, fmtCtx, targetStreamIndex);	// 这个帧的时间线
			int len = static_cast<int>(decodeMissionMap.getValueRef(path).data.at(sec).size());

			step_01(swrCtx, frame, out_channels, out_format, decodeMissionMap.getValueRef(path).data.at(sec).unsafe_ref(), *decodeMissionMap.getValueRef(path).data.at(sec).tMtx, tempBuffer, maxOutSamplesFromBuffer);

		}

	}


	// FFmpeg + SDL 配置
	void FFmpegDecode_2(
		std::string pathStr,
		DecodeMission& dm,
		AVFormatContext* fmtCtx,
		int targetStreamIndex,
		AVCodecContext* codecCtx,
		SwrContext* swrCtx,
		AVPacket* packet,						// 5
		AVFrame* frame,
		SDL_AudioSpec& outputDeviceAudioSpec,
		double startTimeSec,					// 10
		int tempBufferSize = 192000				// 11
	) {
		FFmpegDecode_2(pathStr, dm, fmtCtx, targetStreamIndex, codecCtx, swrCtx, packet, frame,
			outputDeviceAudioSpec.channels, SDLFormatToAV(outputDeviceAudioSpec.format), startTimeSec, tempBufferSize);
	}

	// FFmpeg + SDL 内处理获得配置
	void FFmpegDecode_2(
		std::string pathStr,
		DecodeMission& dm,
		AVFormatContext* fmtCtx,
		int targetStreamIndex,
		AVCodecContext* codecCtx,
		SwrContext* swrCtx,
		AVPacket* packet,				// 5
		AVFrame* frame,
		double startTimeSec,
		int tempBufferSize = 192000		// 10
	) {
		// 以 SDL 的方式获得设备配置
		SDL_AudioSpec outputDeviceAudioSpec;
		SDL_AudioSpec spec;
		spec.freq = 44100;
		spec.format = AUDIO_S16SYS;
		spec.channels = 2;
		spec.samples = 1024;
		spec.callback = nullptr;
		spec.userdata = nullptr;

		auto d = SDL_OpenAudioDevice(nullptr, 0, &spec, &outputDeviceAudioSpec, 0);

		FFmpegDecode_2(pathStr, dm, fmtCtx, targetStreamIndex, codecCtx, swrCtx, packet, frame, outputDeviceAudioSpec.channels, SDLFormatToAV(outputDeviceAudioSpec.format), startTimeSec, tempBufferSize);

		SDL_CloseAudioDevice(d);
	}


	void step_01(
		SwrContext* swrCtx,
		AVFrame* frame,
		int out_channels,
		AVSampleFormat out_format,
		std::vector<uint8_t>& result,	// 5
		std::shared_mutex& mtx,
		std::vector<uint8_t>& tempBuffer,
		int maxOutSample
	) {
		uint8_t* outArr[1] = { tempBuffer.data() };
		int outSamples = swr_convert(
			swrCtx,
			outArr,
			maxOutSample,
			(const uint8_t**)frame->extended_data,
			frame->nb_samples
		);

		if (outSamples > 0) {
			int outDataSize = av_samples_get_buffer_size(
				nullptr,
				out_channels,
				outSamples,
				out_format,
				1
			);
			std::unique_lock<std::shared_mutex> writeLock(mtx);
			result.insert(result.end(), tempBuffer.data(), tempBuffer.data() + outDataSize);
		}
	}

public:


	AVSampleFormat SDLFormatToAV(SDL_AudioFormat fmt) {
		switch (fmt) {
		case AUDIO_U8: return AV_SAMPLE_FMT_U8;
		case AUDIO_S8: return AV_SAMPLE_FMT_S16; // 8.0 没有 AV_SAMPLE_FMT_S8;
		case AUDIO_S16SYS: return AV_SAMPLE_FMT_S16;
		case AUDIO_S32SYS: return AV_SAMPLE_FMT_S32;
		case AUDIO_F32SYS: return AV_SAMPLE_FMT_FLT;
		default: { return AV_SAMPLE_FMT_NONE; }
		}
	}

	// 将 SDL 的 channel 转换为 ffmpeg 的 channel
	void SDLChannelsToAV(Uint8 channels, AVChannelLayout* layout) {
		av_channel_layout_uninit(layout); // 初始化
		if (channels == 1) {
			av_channel_layout_from_mask(layout, AV_CH_LAYOUT_MONO);
		}
		else if (channels == 2) {
			av_channel_layout_from_mask(layout, AV_CH_LAYOUT_STEREO);
		}
		else {
			av_channel_layout_default(layout, channels); // fallback
		}
	}

	// 获得 frame 属于哪一秒
	static int getFrameSecond(AVFrame* frame, AVFormatContext* fmtCtx, int streamIndex) {
		if (!frame || !fmtCtx || streamIndex < 0 || static_cast<unsigned int>(streamIndex) >= fmtCtx->nb_streams)
			return -1;  // 参数错误

		AVStream* stream = fmtCtx->streams[streamIndex];
		if (!stream) return -1;

		// 优先使用 pts，其次使用 best_effort_timestamp
		int64_t pts = AV_NOPTS_VALUE;
		if (frame->pts != AV_NOPTS_VALUE)
			pts = frame->pts;
		else if (frame->best_effort_timestamp != AV_NOPTS_VALUE)
			pts = frame->best_effort_timestamp;
		else
			return -1;  // 无有效时间戳

		// 将 pts 转为秒（浮点）
		double seconds = av_rescale_q(
			pts,
			stream->time_base,
			AVRational{ 1, 1000 }   // 转为毫秒单位
		) / 1000.0;

		if (seconds < 0.0) return -1;

		return static_cast<int>(seconds);
	}


};




class CLIScreen {	// Screen -> (n) table -> (n) column
public:
	class CLITable {
	public:
		/* (一行能不能播完		&	允不允许换行):
		 * ( 能播完 Y	& 允许换行 Y		): 不换行, 需要为未使用的地方留白
		 * ( 能播完 Y	& 不允许换行 N	): 不换行, 需要为未使用的地方留白
		 * ( 不能播完 Y	& 允许换行 Y		): 换行, 需要计算需要使用几行
		 * ( 不能播完 Y	& 不允许换行 N	): 不换行, 播放至上限后后续不播放
		 */


		std::pair<int, int> coordinate;
		std::pair<int, int> size;
		// ================ constructor ================

		CLITable() = default;
		// ================ operator ================

		// ================ method ================


	};
	BiMap<Node*, CLITable*> connect;

};

