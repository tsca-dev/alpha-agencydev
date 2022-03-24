import type {NextPage} from 'next'
import Link from 'next/link';
import {useRouter} from "next/router";

const Home: NextPage = () => {

  let router = useRouter();
  if (typeof document !== "undefined") {
    router.push("/books");
  }

  return (<div>
    <p>Your browser must support JavaScript for this application to run.</p>
    <Link href={"/books"}><a>List of available TSCA Books of Contract</a></Link>
  </div>);
}

export default Home
