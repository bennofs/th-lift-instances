green='\e[0;32m'
red='\e[0;31m'
nc='\e[0m' # No Color

function step {
  echo -e "${green}$1 ...${nc}"
  bash /dev/stdin || exit 1
}

function step_suppress {
  echo -ne "${green}$1 ... ${nc}"
  tmp=$(mktemp)
  bash /dev/stdin &> $tmp && echo -e "${green}Done${nc}" || (
    echo -e "${red}Failed${nc}"
    echo "Output: "
    cat $tmp
    exit 1
  )
}
