sayHello:
		echo "hello"

install-ollama:
	curl -fsSL https://ollama.com/install.sh | sh
	ollama pull smollm:360m
